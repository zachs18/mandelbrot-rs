use crate::common::{Exists, WatchedInner};
use std::{
    cell::RefCell,
    future::Future,
    rc::{Rc, Weak},
    task::Poll,
};

#[derive(Debug, Default)]
pub struct Watched<T> {
    inner: RefCell<WatchedInner<T>>,
}

impl<T> Watched<T> {
    pub fn set(&self, value: T) {
        let mut inner = self.inner.borrow_mut();
        inner.generation = inner
            .generation
            .checked_add(1)
            .expect("generation overflow");
        inner.value = value;
        for waker in inner.wakers.drain(..) {
            waker.wake();
        }
    }

    pub fn update(&self, update_fn: impl FnOnce(&mut T) -> bool) {
        let mut inner = self.inner.borrow_mut();
        let updated = update_fn(&mut inner.value);

        if updated {
            inner.generation = inner
                .generation
                .checked_add(1)
                .expect("generation overflow");
            for waker in inner.wakers.drain(..) {
                waker.wake();
            }
        }
    }

    pub fn new(value: T) -> Rc<Self> {
        Rc::new(Watched {
            inner: RefCell::new(WatchedInner {
                value,
                generation: 0,
                wakers: vec![],
            }),
        })
    }
}

impl<'a, T: Clone + 'a> Watched<T> {
    pub fn watch(self: &Rc<Self>) -> Watcher<'a, T> {
        self.watch_with_borrow(Clone::clone, Some(|this| {
            let this = this as *const _ as *const T;
            unsafe {&*this}
        }))
    }

    pub fn get(&self) -> T {
        self.inner.borrow().value.clone()
    }
}

impl<'a, T: 'a> Watched<T> {
    pub fn watch_with<U, F: Fn(&T) -> U + 'a>(self: &Rc<Self>, func: F) -> Watcher<'a, U> {
        self.watch_with_borrow(func, None)
    }

    fn watch_with_borrow<U, F: Fn(&T) -> U + 'a>(
        self: &Rc<Self>, func: F,
        borrow_fn: Option<for<'b> fn(&'b(dyn Exists + 'a)) -> &'b U>,
    ) -> Watcher<'a, U> {
        let inner = self.inner.borrow();
        let generation = inner.generation;
        let poll_fn: Rc<
            dyn Fn(&(dyn Exists + 'a), &mut usize, &mut std::task::Context) -> Poll<U>,
        > = Rc::new(move |this, current_generation, cx| {
            let this = this as *const _ as *const Self;
            let this = unsafe { &*this };
            let inner = this.inner.borrow();
            let generation = inner.generation;
            if generation > *current_generation {
                let value = func(&inner.value);
                drop(inner);
                *current_generation = generation;
                Poll::Ready(value)
            } else {
                // Double-checked locking idiom
                drop(inner);

                let mut inner = this.inner.borrow_mut();
                let generation = inner.generation;
                if generation > *current_generation {
                    let value = func(&inner.value);
                    drop(inner);
                    *current_generation = generation;
                    Poll::Ready(value)
                } else {
                    inner.wakers.push(cx.waker().clone());
                    Poll::Pending
                }
            }
        });
        Watcher {
            generation,
            watched: Rc::downgrade(&self) as _,
            borrow_fn,
            poll_fn,
        }
    }
}

#[derive(Clone)]
pub struct Watcher<'a, T> {
    generation: usize,
    watched: Weak<dyn Exists + 'a>,
    borrow_fn: Option<for<'b> fn(&'b (dyn Exists + 'a)) -> &'b T>,
    poll_fn: Rc<dyn Fn(&(dyn Exists + 'a), &mut usize, &mut std::task::Context) -> Poll<T> + 'a>,
}

impl<'a, T> Watcher<'a, T> {
    pub fn with<F: FnOnce(&T) -> R, R>(&self, f: F) -> Result<R, WatchedDropped> {
        let borrow_fn = self.borrow_fn.expect("Cannot call with on mapped Watcher.");
        match self.watched.upgrade() {
            Some(watched) => {
                Ok(f(borrow_fn(&*watched)))
            }
            None => Err(WatchedDropped),
        }
    }

    /// Postfix replacement for (&mut watcher).await
    #[inline]
    pub fn watch(&mut self) -> &mut Self {
        self
    }
}

pub struct WatchedDropped;

impl<'a, T: Clone> Future for &mut Watcher<'a, T> {
    type Output = Result<T, WatchedDropped>;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        match self.watched.upgrade() {
            Some(watched) => {
                // Can't split borrows through Pin
                let this = &mut **self;
                (this.poll_fn)(&*watched, &mut this.generation, cx).map(Ok)
            }
            None => Poll::Ready(Err(WatchedDropped)),
        }
    }
}
