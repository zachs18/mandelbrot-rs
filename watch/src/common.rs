use std::{
    ops::{Deref, DerefMut},
    task::Waker,
};

pub trait Exists {}
impl<T: ?Sized> Exists for T {}

#[derive(Debug, Default)]
pub(crate) struct WatchedInner<T> {
    pub(crate) value: T,
    pub(crate) generation: usize,
    pub(crate) wakers: Vec<Waker>,
}

impl<T> Drop for WatchedInner<T> {
    fn drop(&mut self) {
        for waker in self.wakers.drain(..) {
            waker.wake();
        }
    }
}

impl<T> Deref for WatchedInner<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for WatchedInner<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
