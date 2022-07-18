use std::{
    cell::{Cell, RefCell},
    rc::Rc,
    str::FromStr, sync::Arc,
};

use blocking::unblock;
use f128::f128;
use gtk::{
    gdk::{EventMask, EventType},
    gdk_pixbuf::Pixbuf,
    glib::Bytes,
    prelude::{
        ApplicationExt, ApplicationExtManual, BuilderExtManual, CssProviderExt,
        GdkContextExt, WidgetExtManual,
    },
    traits::{EntryExt, GtkApplicationExt, StyleContextExt, WidgetExt, ButtonExt, ToggleButtonExt, TextViewExt, TextBufferExt},
    Application, Builder, CssProvider, DrawingArea, EditableSignals, Entry, Inhibit, StyleContext,
    Window, Button, RadioButton, TextView, Grid,
};
#[cfg(not(feature = "custom_fractals"))]
use gtk::traits::ContainerExt;
use image::RgbImage;
use loader::AssertSendSyncExt;
use num_complex::Complex;
use num_traits::Zero;
use watch::local::Watched;

mod generation;

use crate::generation::{Config, Precision, generate, Fractal};

fn main() {
    let config = Config::default();

    // Register and include resources
    gtk::gio::resources_register_include!("compiled.gresource")
        .expect("Failed to register resources.");

    // gui
    // Create a new application
    let app = Application::builder()
        .application_id("com.github.zachs18.mandelbrot")
        .build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_logic(config));

    // Run the application
    app.run_with_args::<&str>(&[]);
}

fn make_reader_entry<T: FromStr>(entry: &Entry, update_fn: impl Fn(T) + 'static) {
    entry.connect_changed(move |this: &Entry| {
        let ctx = this.style_context();
        if let Ok(val) = this.text().parse() {
            update_fn(val);
            ctx.remove_class("bad");
        } else {
            ctx.add_class("bad");
        }
    });
}

fn build_logic(config: Config) -> impl Fn(&gtk::Application) {
    let config = RefCell::new(Some(config));
    move |app: &gtk::Application| {
        struct State {
            config: Rc<Watched<Config>>,
            centerx_entry: Entry,
            centery_entry: Entry,
            scale_entry: Entry,
            max_iterations_entry: Entry,
            hue_scale_entry: Entry,

            mandelbrot_radiobutton: RadioButton,
            burning_ship_radiobutton: RadioButton,
            #[cfg(feature = "custom_fractals")]
            custom_fractal_radiobutton: RadioButton,

            #[cfg(feature = "custom_fractals")]
            custom_fractal_text_view: TextView,

            f32_radiobutton: RadioButton,
            f64_radiobutton: RadioButton,
            f128_radiobutton: RadioButton,

            // img: Watcher<'static, Option<RgbImage>>,
            img: Rc<RefCell<Option<RgbImage>>>,
        }

        impl State {
            fn update_widgets(&self) {
                let config = self.config.get();

                self.centerx_entry.set_text(&format!("{}", config.center.0));
                self.centery_entry.set_text(&format!("{}", config.center.1));
                self.scale_entry.set_text(&format!("{}", config.zoom));
                self.max_iterations_entry.set_text(&format!("{}", config.max_iterations));
                self.hue_scale_entry.set_text(&format!("{}", config.hue_scale));

                match config.fractal {
                    Fractal::Mandelbrot => self.mandelbrot_radiobutton.set_active(true),
                    Fractal::BurningShip => self.burning_ship_radiobutton.set_active(true),
                    #[cfg(feature = "custom_fractals")]
                    Fractal::Custom { .. } => self.custom_fractal_radiobutton.set_active(true),
                }

                match config.precision {
                    Precision::Single => self.f32_radiobutton.set_active(true),
                    Precision::Double => self.f64_radiobutton.set_active(true),
                    Precision::Quad => self.f128_radiobutton.set_active(true),
                }
            }
        }

        let config = config.take().expect("activate should only be called once");

        let builder = Builder::from_resource("/zachs18/mandelbrot/window.ui");

        macro_rules! get_widgets {
            ( $( $(#[$($attr:tt)*])* $name:ident: $ty:ty;)* ) => {
                $(
                    $(#[$($attr)*])*
                    let $name: $ty = builder.object(stringify!($name)).unwrap();
                )*
            }
        }

        get_widgets!{
            window: Window;
            #[allow(unused_variables)]
            control_grid: Grid;
            #[allow(unused_variables)]
            custom_fractal_text_view_box: gtk::Box;

            centerx_entry: Entry;
            centery_entry: Entry;
            scale_entry: Entry;
            max_iterations_entry: Entry;
            hue_scale_entry: Entry;

            mandelbrot_radiobutton: RadioButton;
            burning_ship_radiobutton: RadioButton;
            #[cfg(feature = "custom_fractals")]
            custom_fractal_radiobutton: RadioButton;

            #[cfg(feature = "custom_fractals")]
            custom_fractal_text_view: TextView;

            f32_radiobutton: RadioButton;
            f64_radiobutton: RadioButton;
            f128_radiobutton: RadioButton;

            reset_button: Button;
            drawing_area: DrawingArea;
            progress_text_view: TextView;
        };

        #[cfg(not(feature = "custom_fractals"))]
        {
            control_grid.remove(&custom_fractal_radiobutton);
            control_grid.remove(&custom_fractal_text_view_box);
        }

        let screen = gtk::gdk::Screen::default().unwrap();
        let entry_red_background_style = CssProvider::new();
        entry_red_background_style
            .load_from_data(b".bad { background-image: image(red); }")
            .unwrap();
        StyleContext::add_provider_for_screen(
            &screen,
            &entry_red_background_style,
            gtk::STYLE_PROVIDER_PRIORITY_USER,
        );

        // let img_watched = Watched::new(None::<RgbImage>);
        // let img_watcher = img_watched.watch();
        let img_rc: Rc<RefCell<Option<RgbImage>>> = Rc::default();

        let state = Rc::new(State {
            config: Watched::new(config),
            centerx_entry,
            centery_entry,
            scale_entry,
            max_iterations_entry,
            hue_scale_entry,

            mandelbrot_radiobutton,
            burning_ship_radiobutton,
            #[cfg(feature = "custom_fractals")]
            custom_fractal_radiobutton,

            #[cfg(feature = "custom_fractals")]
            custom_fractal_text_view,

            f32_radiobutton,
            f64_radiobutton,
            f128_radiobutton,
            img: img_rc.clone(),
        });

        // Initialize entries to initial (default) config values.
        state.update_widgets();

        macro_rules! make_reader_entry_callback {
            ( $field:ident $(. $idx:tt)?: $t:ty ) => {{
                let state = Rc::clone(&state);
                move |val: $t| {
                    eprintln!("TODO: If this callback is a result of a programmatic change, don't update(?)");
                    state.config.update(|config| {
                        config.$field $(.$idx)? = val;
                        true
                    });
                }
            }};
        }

        {
            make_reader_entry(
                &state.centerx_entry,
                make_reader_entry_callback!(center.0: f64),
            );
            make_reader_entry(
                &state.centery_entry,
                make_reader_entry_callback!(center.1: f64),
            );
            make_reader_entry(&state.scale_entry, make_reader_entry_callback!(zoom: f64));
            make_reader_entry(
                &state.max_iterations_entry,
                make_reader_entry_callback!(max_iterations: u32),
            );
            make_reader_entry(
                &state.hue_scale_entry,
                make_reader_entry_callback!(hue_scale: f64),
            );
            reset_button.connect_clicked({
                let state = Rc::clone(&state);
                move |_this| {
                    state.config.update(|config| {
                        let size = config.size;
                        *config = Config { size, ..Config::default() };
                        true
                    });
                    state.update_widgets();
                }
            });
        }

        macro_rules! setup_enum_radiobutton_callback {
            ( $button:ident: $field:ident = $value:expr ) => {
                state.$button.connect_toggled({
                    let state = Rc::clone(&state);
                    move |this| {
                        if !this.is_active() { return; }
                        state.config.update(|config| {
                            config.$field = $value;
                            true
                        });
                    }
                });
            };
        }

        setup_enum_radiobutton_callback!(
            f32_radiobutton: precision = Precision::Single
        );
        setup_enum_radiobutton_callback!(
            f64_radiobutton: precision = Precision::Double
        );
        setup_enum_radiobutton_callback!(
            f128_radiobutton: precision = Precision::Quad
        );

        setup_enum_radiobutton_callback!(
            mandelbrot_radiobutton: fractal = Fractal::Mandelbrot
        );
        setup_enum_radiobutton_callback!(
            burning_ship_radiobutton: fractal = Fractal::BurningShip
        );
        state.custom_fractal_radiobutton.connect_toggled({
            let state = Rc::clone(&state);
            let buffer = state.custom_fractal_text_view.buffer().unwrap();
            move |this| {
                if !this.is_active() { return; }
                state.config.update(|config| {
                    let text = buffer.text(&buffer.start_iter(), &buffer.end_iter(), false).unwrap();
                    let handle = match compile_in_memory::compile(
                        "gcc",
                        &text,
                        "c",
                        compile_in_memory::OptimizationLevel::Two,
                        false,
                    ) {
                        Ok(handle) => Arc::new(handle),
                        Err(_) => todo!(),
                    };
                    eprintln!("TODO: Update these when the text changes.");
                    eprintln!("TODO: handle errors gracefully.");
                    eprintln!("TODO: make a minilanguage/parser and generate the three functions");

                    type Callback<T> = extern "C" fn(*mut Complex<T>, *const Complex<T>, *const Complex<T>);

                    let single = handle.sym_func_owned::<Callback<f32>>(loader::c_str!("func32")).unwrap();
                    let double = handle.sym_func_owned::<Callback<f64>>(loader::c_str!("func64")).unwrap();
                    let quad = handle.sym_func_owned::<Callback<f128>>(loader::c_str!("func128")).unwrap();

                    let single = unsafe { single.assert_shared().assert_send_sync() };
                    let double = unsafe { double.assert_shared().assert_send_sync() };
                    let quad = unsafe { quad.assert_shared().assert_send_sync() };

                    let single = move |c: Complex<f32>| {
                        let single = single.clone();
                        Box::new(move |z: Complex<f32>| {
                            let mut result = Complex::zero();
                            single(&mut result, &c, &z);
                            result
                        }) as Box<dyn FnMut(Complex<f32>) -> Complex<f32> + 'static>
                    };
                    let double = move |c: Complex<f64>| {
                        let double = double.clone();
                        Box::new(move |z: Complex<f64>| {
                            let mut result = Complex::zero();
                            double(&mut result, &c, &z);
                            result
                        }) as Box<dyn FnMut(Complex<f64>) -> Complex<f64> + 'static>
                    };
                    let quad = move |c: Complex<f128>| {
                        let quad = quad.clone();
                        Box::new(move |z: Complex<f128>| {
                            let mut result = Complex::zero();
                            quad(&mut result, &c, &z);
                            result
                        }) as Box<dyn FnMut(Complex<f128>) -> Complex<f128> + 'static>
                    };

                    config.fractal = Fractal::Custom {
                        single: Arc::new(single),
                        double: Arc::new(double),
                        quad: Arc::new(quad),
                    };
                    true
                });
            }
        });

        fn coordinate_convert(
            center: (f64, f64),
            zoom: f64,
            event_position: (f64, f64),
            event_window: (f64, f64),
        ) -> (f64, f64) {
            // Find scroll position in coordinate space
            let x = center.0 + (event_position.0 - event_window.0 / 2.0) / zoom;
            let y = center.1 + (event_position.1 - event_window.1 / 2.0) / zoom;
            (x, y)
        }

        let zoom_to = {
            let state = Rc::clone(&state);
            move |zoom_location: (f64, f64), scale_factor: f64| {
                state.config.update(|config| {
                    let Config {
                        center: (cx, cy),
                        zoom,
                        ..
                    } = config;
                    let (zoom_x, zoom_y) = zoom_location;

                    // https://www.desmos.com/calculator/vvpvpvxnhi
                    *cx = (*cx - zoom_x) * scale_factor + zoom_x;
                    *cy = (*cy - zoom_y) * scale_factor + zoom_y;
                    *zoom /= scale_factor;
                    true
                });

                state.update_widgets();
            }
        };

        #[derive(Debug, Clone, Copy)]
        enum Move {
            Relative(f64, f64),
            Absolute(f64, f64),
        }

        let move_center = {
            let state = Rc::clone(&state);
            move |move_: Move| {
                state.config.update(|config| {
                    let Config {
                        center: (cx, cy), ..
                    } = config;
                    match move_ {
                        Move::Relative(dx, dy) => {
                            *cx += dx;
                            *cy += dy;
                        }
                        Move::Absolute(x, y) => {
                            *cx = x;
                            *cy = y;
                        }
                    }
                    true
                });

                state.update_widgets();
            }
        };

        drawing_area.add_events(
            EventMask::SCROLL_MASK
                | EventMask::SMOOTH_SCROLL_MASK
                | EventMask::BUTTON_PRESS_MASK
                | EventMask::BUTTON_RELEASE_MASK
                | EventMask::BUTTON_MOTION_MASK,
        );

        drawing_area.connect_scroll_event({
            let state = Rc::clone(&state);
            let zoom_to = zoom_to.clone();
            move |this, event| {
                let scroll_position = event.position();
                let scroll_window = this.allocation();
                let scroll_window = (scroll_window.width() as f64, scroll_window.height() as f64);

                let Config { center, zoom, .. } = state.config.get();

                let zoom_location =
                    coordinate_convert(center, zoom, scroll_position, scroll_window);

                let (_dx, dy) = event.delta();
                let scale_factor = if dy < 0.0 {
                    // "Up" = zoom in
                    0.95
                } else {
                    1.0 / 0.95
                };

                zoom_to(zoom_location, scale_factor);

                Inhibit(true)
            }
        });

        #[derive(Debug, Clone, Copy)]
        struct DragState {
            drag_position: (f64, f64),
        }

        let drag_state = Rc::new(Cell::new(None::<DragState>));

        drawing_area.connect_button_press_event({
            let state = Rc::clone(&state);
            let drag_state = Rc::clone(&drag_state);
            move |this, event| {
                let press_position = event.position();
                let press_window = this.allocation();
                let press_window = (press_window.width() as f64, press_window.height() as f64);

                let Config { center, zoom, .. } = state.config.get();

                // Find press position in coordinate space
                let press_location = coordinate_convert(center, zoom, press_position, press_window);

                if event.event_type() == EventType::ButtonPress {
                    drag_state.set(Some(DragState {
                        drag_position: press_location,
                    }));
                } else if event.event_type() == EventType::DoubleButtonPress {
                    let scale_factor = if event.button() == 1 {
                        // Double primary click == zoom in
                        0.5
                    } else {
                        // Double alternate click == zoom out
                        2.0
                    };
                    zoom_to(press_location, scale_factor);
                }
                Inhibit(false)
            }
        });

        drawing_area.connect_button_release_event({
            let drag_state = Rc::clone(&drag_state);
            move |_this, _event| {
                drag_state.set(None);
                Inhibit(false)
            }
        });

        drawing_area.connect_motion_notify_event({
            let state = Rc::clone(&state);
            let drag_state = Rc::clone(&drag_state);
            move |this, event| {
                if let Some(DragState { drag_position }) = drag_state.get() {
                    let press_position = event.position();
                    let press_window = this.allocation();
                    let press_window = (press_window.width() as f64, press_window.height() as f64);

                    let Config { center, zoom, .. } = state.config.get();

                    // Find press position in coordinate space
                    let press_location =
                        coordinate_convert(center, zoom, press_position, press_window);

                    let drag_offset_x = drag_position.0 - press_location.0;
                    let drag_offset_y = drag_position.1 - press_location.1;

                    move_center(Move::Relative(drag_offset_x, drag_offset_y));
                }
                Inhibit(false)
            }
        });

        eprintln!("TODO: Parameter space vs Seed space (see https://youtu.be/LqbZpur38nw)");

        window.connect_destroy({
            let _state = Rc::clone(&state);
            move |_this| {
                eprintln!("TODO: handle quitting?");
                // state.quitting = true;
            }
        });

        gtk::glib::MainContext::default().spawn_local({
            let mut config = state.config.watch();
            let drawing_area = drawing_area.clone();
            async move {
                let progress_text_buffer = progress_text_view.buffer().unwrap();
                loop {
                    let config = match config.watch().await {
                        Ok(config) => config,
                        Err(_) => {
                            break
                        },
                    };
                    progress_text_buffer.set_text("Working...");
                    let img = unblock(move || generate(config)).await;
                    progress_text_buffer.set_text("Done.");
                    *img_rc.borrow_mut() = Some(img);
                    drawing_area.queue_draw();
                }
            }
        });

        drawing_area.connect_configure_event({
            let state = Rc::clone(&state);
            move |this, _event| {
                state.config.update(|config| {
                    let allocation = this.allocation();
                    config.size = (allocation.width(), allocation.height());
                    true
                });
                false
            }
        });

        drawing_area.connect_damage_event({
            let state = Rc::clone(&state);
            move |this, _event| {
                state.config.update(|config| {
                    let allocation = this.allocation();
                    config.size = (allocation.width(), allocation.height());
                    true
                });
                false
            }
        });

        drawing_area.connect_draw({
            let state = Rc::clone(&state);
            move |_this, ctx| {
                let img = state.img.borrow();
                if let Some(img) = img.as_ref() {
                    let width = img.width().try_into().expect("image too wide");
                    let height = img.height().try_into().expect("image too tall");
                    let data = Bytes::from(&**img);
                    let pixbuf = Pixbuf::from_bytes(
                        &data,
                        gtk::gdk_pixbuf::Colorspace::Rgb,
                        false,
                        8,
                        width,
                        height,
                        width * 3,
                    );
                    ctx.set_source_pixbuf(&pixbuf, 0.0, 0.0);
                    // ctx.set_source_rgb(1.0, 0.0, 0.0);
                    ctx.rectangle(0.0, 0.0, width as f64, height as f64);
                    ctx.fill().unwrap();
                } else {
                    dbg!("Failed to get img");
                }
                eprintln!("TODO: when zooming, use a zoomed version of old image while new image is being generated");
                Inhibit(true)
            }
        });

        app.add_window(&window);

        window.show_all();
    }
}
