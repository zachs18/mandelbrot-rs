use std::{
    cell::{Cell, RefCell},
    ops::ControlFlow,
    rc::Rc,
    str::FromStr,
};

use gtk::{
    gdk::{EventMask, EventType},
    gdk_pixbuf::Pixbuf,
    glib::Bytes,
    prelude::{
        ApplicationExt, ApplicationExtManual, BuilderExtManual, Continue, CssProviderExt,
        GdkContextExt, WidgetExtManual,
    },
    traits::{EntryExt, GtkApplicationExt, StyleContextExt, WidgetExt, ButtonExt},
    Application, Builder, CssProvider, DrawingArea, EditableSignals, Entry, Inhibit, StyleContext,
    Window, Button,
};
use image::{Rgb, RgbImage};
use num_complex::Complex;

#[derive(Debug, Clone)]
struct Config {
    max_iterations: u32,
    center: (f64, f64),
    zoom: f64,
    hue_scale: f64,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_iterations: 1000,
            center: (-0.75, 0.0),
            zoom: 192.0,
            hue_scale: 60.0,
        }
    }
}

// impl Config {
//     fn parse() -> Result<Self, String> {
//         let mut config = Config::default();
//         let mut args = std::env::args();
//         let _executable = args.next();
//         while let Some(arg) = args.next() {
//             match &*arg {
//                 "-g" | "--gui" => {
//                     config.gui = true;
//                 }
//                 "-o" => {
//                     config.filename = Some(unwrap_or!(
//                         args.next(),
//                         return Err(format!("Expected argument for -o option")),
//                     ));
//                 }
//                 "-i" => {
//                     let iterations = unwrap_or!(
//                         args.next(),
//                         return Err(format!("Expected argument for -o option")),
//                     );
//                     config.max_iterations = unwrap_or!(
//                         iterations.parse::<u32>(),
//                         return Err(format!("Invalid number of iterations: {e:?}")),
//                         with_error(e)
//                     );
//                 }
//                 arg if arg.contains('x') => {
//                     let idx = arg.find("x").expect("contains 'x'");
//                     let (w, h) = arg.split_at(idx);
//                     let h = &h[1..];
//                     let w = unwrap_or!(
//                         w.parse::<u32>(),
//                         return Err(format!("Invalid width: {w:?}")),
//                     );
//                     let h = unwrap_or!(
//                         h.parse::<u32>(),
//                         return Err(format!("Invalid height: {h:?}")),
//                     );
//                     config.width = w;
//                     config.height = h;
//                 }
//                 _ => {
//                     return Err(format!("Unexpected argument: {arg:?}"));
//                 }
//             }
//         }
//         Ok(config)
//     }
// }

fn fraction_to_hue(x: f64) -> Rgb<u8> {
    let x = x % 1.0;
    let x = (x * 1530.0) as u32;
    #[allow(overlapping_range_endpoints)]
    match x {
        0..=255 => Rgb([255, x as u8, 0]),
        255..=510 => Rgb([(510 - x) as u8, 255, 0]),
        510..=765 => Rgb([0, 255, (x - 510) as u8]),
        765..=1020 => Rgb([0, (1020 - x) as u8, 255]),
        1020..=1275 => Rgb([(x - 1020) as u8, 0, 255]),
        1275..=1530 => Rgb([255, 0, (1530 - x) as u8]),
        _ => unreachable!("x <= 1530"),
    }
}

fn generate(config: &Config, width: i32, height: i32) -> RgbImage {
    let &Config {
        max_iterations,
        center: (cx, cy),
        zoom,
        hue_scale,
        ..
    } = config;
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    let mut img = image::RgbImage::new(width, height);
    for y in 0..height {
        let im = cy + (y as f64 - height as f64 / 2.0) / zoom;
        for x in 0..width {
            let re = cx + (x as f64 - width as f64 / 2.0) / zoom;
            let c = Complex { re, im };
            let f = |z: Complex<f64>| z * z + c;
            let iterations =
                (0..max_iterations).try_fold(Complex::new(0.0, 0.0), |val, iteration| {
                    let val = f(val);
                    if val.norm_sqr() > 4.0 {
                        ControlFlow::Break(iteration)
                    } else {
                        ControlFlow::Continue(val)
                    }
                });

            *img.get_pixel_mut(x, y) = match iterations {
                ControlFlow::Break(it) => {
                    fraction_to_hue((it as f64 / max_iterations as f64) * hue_scale)
                }
                ControlFlow::Continue(_) => image::Rgb([0, 0, 0]),
            };
        }
    }

    img
}

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
            changed: bool,
            quitting: bool,
            config: Config,
            centerx_entry: Entry,
            centery_entry: Entry,
            scale_entry: Entry,
            max_iterations_entry: Entry,
            hue_scale_entry: Entry,
            img: Option<RgbImage>,
        }

        let config = config.take().expect("activate should only be called once");

        let builder = Builder::from_resource("/zachs18/mandelbrot/window.ui");
        let window: Window = builder.object("window").unwrap();
        let centerx_entry: Entry = builder.object("centerx_entry").unwrap();
        let centery_entry: Entry = builder.object("centery_entry").unwrap();
        let scale_entry: Entry = builder.object("scale_entry").unwrap();
        let max_iterations_entry: Entry = builder.object("max_iterations_entry").unwrap();
        let hue_scale_entry: Entry = builder.object("hue_scale_entry").unwrap();
        let reset_button: Button = builder.object("reset_button").unwrap();
        let drawing_area: DrawingArea = builder.object("drawing_area").unwrap();

        centerx_entry.set_text(&format!("{}", config.center.0));
        centery_entry.set_text(&format!("{}", config.center.1));
        scale_entry.set_text(&format!("{}", config.zoom));
        max_iterations_entry.set_text(&format!("{}", config.max_iterations));
        hue_scale_entry.set_text(&format!("{}", config.hue_scale));

        let state = Rc::new(RefCell::new(State {
            changed: true,
            quitting: false,
            config,
            centerx_entry,
            centery_entry,
            scale_entry,
            max_iterations_entry,
            hue_scale_entry,
            img: None,
        }));

        macro_rules! make_reader_entry_callback {
            ( $range:ident $(. $idx:tt)?: $t:ty ) => {{
                let state = Rc::clone(&state);
                move |val: $t| {
                    // If this callback is a result of a programmatic change, don't update.
                    if let Ok(mut state) = state.try_borrow_mut() {
                        state.config.$range $(.$idx)? = val;
                        state.changed = true;
                    }
                }
            }};
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

        {
            let state_ = state.borrow();
            make_reader_entry(
                &state_.centerx_entry,
                make_reader_entry_callback!(center.0: f64),
            );
            make_reader_entry(
                &state_.centery_entry,
                make_reader_entry_callback!(center.1: f64),
            );
            make_reader_entry(&state_.scale_entry, make_reader_entry_callback!(zoom: f64));
            make_reader_entry(
                &state_.max_iterations_entry,
                make_reader_entry_callback!(max_iterations: u32),
            );
            make_reader_entry(
                &state_.hue_scale_entry,
                make_reader_entry_callback!(hue_scale: f64),
            );
            reset_button.connect_clicked({
                let state = Rc::clone(&state);
                move |_this| {
                    let mut state = state.borrow_mut();
                    state.config = Config::default();
                    state.changed = true;
                }
            });
        }

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
                let mut state = state.borrow_mut();
                let Config {
                    center: (cx, cy),
                    zoom,
                    ..
                } = &mut state.config;
                let (zoom_x, zoom_y) = zoom_location;

                // https://www.desmos.com/calculator/vvpvpvxnhi
                *cx = (*cx - zoom_x) * scale_factor + zoom_x;
                *cy = (*cy - zoom_y) * scale_factor + zoom_y;
                *zoom /= scale_factor;
                state.changed = true;

                state
                    .centerx_entry
                    .set_text(&format!("{}", state.config.center.0));
                state
                    .centery_entry
                    .set_text(&format!("{}", state.config.center.1));
                state
                    .scale_entry
                    .set_text(&format!("{}", state.config.zoom));
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
                let mut state = state.borrow_mut();
                let Config {
                    center: (cx, cy), ..
                } = &mut state.config;
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
                state.changed = true;

                state
                    .centerx_entry
                    .set_text(&format!("{}", state.config.center.0));
                state
                    .centery_entry
                    .set_text(&format!("{}", state.config.center.1));
                state
                    .scale_entry
                    .set_text(&format!("{}", state.config.zoom));
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

                let Config { center, zoom, .. } = state.borrow().config;

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

                let Config { center, zoom, .. } = state.borrow().config;

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

                    let Config { center, zoom, .. } = state.borrow().config;

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

        window.connect_destroy({
            let state = Rc::clone(&state);
            move |_this| {
                let mut state = state.borrow_mut();
                state.quitting = true;
            }
        });

        gtk::glib::timeout_add_local(std::time::Duration::from_millis(10), {
            let state = Rc::clone(&state);
            let drawing_area = drawing_area.clone();
            move || {
                let mut state = state.borrow_mut();
                if state.quitting {
                    return Continue(false);
                } else if !state.changed {
                    return Continue(true);
                }
                let config = state.config.clone();
                state.changed = false;
                let allocation = drawing_area.allocation();
                let img = generate(&config, allocation.width(), allocation.height());
                // let width = img.width().try_into().expect("image too wide");
                // let height = img.height().try_into().expect("image too tall");
                // drawing_area.set_size_request(width, height);
                drawing_area.queue_draw();
                state.img = Some(img);
                Continue(true)
            }
        });

        drawing_area.connect_configure_event({
            let state = Rc::clone(&state);
            move |_this, _event| {
                let mut state = state.borrow_mut();
                state.changed = true;
                false
            }
        });

        drawing_area.connect_damage_event({
            let state = Rc::clone(&state);
            move |_this, _event| {
                let mut state = state.borrow_mut();
                state.changed = true;
                false
            }
        });

        drawing_area.connect_draw({
            let state = Rc::clone(&state);
            move |_this, ctx| {
                let state = state.borrow();
                if let Some(img) = state.img.as_ref() {
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
                Inhibit(true)
            }
        });

        app.add_window(&window);

        window.show_all();
    }
}
