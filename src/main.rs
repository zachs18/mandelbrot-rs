use std::{cell::RefCell, fs::File, ops::ControlFlow, rc::Rc, str::FromStr};

use gtk::{
    gdk::EventMask,
    gdk_pixbuf::Pixbuf,
    glib::{clone, Bytes},
    prelude::{
        ApplicationExt, ApplicationExtManual, BuilderExtManual, Continue, CssProviderExt,
        GdkContextExt, WidgetExtManual,
    },
    traits::{EntryExt, GtkApplicationExt, StyleContextExt, WidgetExt},
    Application, Builder, CssProvider, DrawingArea, EditableSignals, Entry, Inhibit, StyleContext, Window,
};
use image::{codecs::pnm::PnmEncoder, ImageEncoder, ImageError, Rgb, RgbImage};
use num_complex::Complex;

mod utils;

#[derive(Debug, Clone)]
struct Config {
    width: u32,
    height: u32,
    max_iterations: u32,
    filename: Option<String>,
    xrange: (f64, f64),
    yrange: (f64, f64),
    gui: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            width: Default::default(),
            height: Default::default(),
            max_iterations: Default::default(),
            filename: Default::default(),
            xrange: (-0.125, 0.125),
            yrange: (0.75, 1.0),
            gui: false,
        }
    }
}

impl Config {
    fn parse() -> Result<Self, String> {
        let mut config = Config::default();
        let mut args = std::env::args();
        let _executable = args.next();
        while let Some(arg) = args.next() {
            match &*arg {
                "-g" | "--gui" => {
                    config.gui = true;
                }
                "-o" => {
                    config.filename = Some(unwrap_or!(
                        args.next(),
                        return Err(format!("Expected argument for -o option")),
                    ));
                }
                "-i" => {
                    let iterations = unwrap_or!(
                        args.next(),
                        return Err(format!("Expected argument for -o option")),
                    );
                    config.max_iterations = unwrap_or!(
                        iterations.parse::<u32>(),
                        return Err(format!("Invalid number of iterations: {e:?}")),
                        with_error(e)
                    );
                }
                arg if arg.contains('x') => {
                    let idx = arg.find("x").expect("contains 'x'");
                    let (w, h) = arg.split_at(idx);
                    let h = &h[1..];
                    let w = unwrap_or!(
                        w.parse::<u32>(),
                        return Err(format!("Invalid width: {w:?}")),
                    );
                    let h = unwrap_or!(
                        h.parse::<u32>(),
                        return Err(format!("Invalid height: {h:?}")),
                    );
                    config.width = w;
                    config.height = h;
                }
                _ => {
                    return Err(format!("Unexpected argument: {arg:?}"));
                }
            }
        }
        Ok(config)
    }
}

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

fn generate(config: &Config) -> RgbImage {
    let &Config {
        width,
        height,
        max_iterations,
        xrange: (minx, maxx),
        yrange: (miny, maxy),
        ..
    } = config;

    let xscale = maxx - minx;
    let yscale = maxy - miny;

    let mut img = image::RgbImage::new(width, height);
    for y in 0..height {
        let im = yscale * (y as f64) / height as f64 + miny;
        for x in 0..width {
            let re = xscale * (x as f64) / width as f64 + minx;
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
                    fraction_to_hue((it as f64 / max_iterations as f64) * 60.0)
                }
                ControlFlow::Continue(_) => image::Rgb([0, 0, 0]),
            };
        }
    }

    img
}

fn main() {
    let config = Config::parse().unwrap();
    if !config.gui {
        let img = generate(&config);

        let filename = config.filename.as_deref().unwrap_or("/dev/stdout");
        dbg!(filename);

        match img.save(filename) {
            Err(ImageError::Unsupported(_)) => {
                // img.save_with_format(filename, image::ImageFormat::Pnm).unwrap();
                let encoder = PnmEncoder::new(File::create(filename).unwrap());
                encoder
                    .write_image(
                        img.as_raw(),
                        img.width(),
                        img.height(),
                        image::ColorType::Rgb8,
                    )
                    .unwrap();
            }
            result => return result.unwrap(),
        };
    } else {
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

fn build_logic(mut config: Config) -> impl Fn(&gtk::Application) {
    config.width = 512;
    config.height = 512;
    let config = RefCell::new(Some(config));
    move |app: &gtk::Application| {
        struct State {
            changed: bool,
            quitting: bool,
            config: Config,
            minx_entry: Entry,
            maxx_entry: Entry,
            miny_entry: Entry,
            maxy_entry: Entry,
            img: Option<RgbImage>,
        }

        let config = config.take().expect("activate should only be called once");

        let builder = Builder::from_resource("/zachs18/mandelbrot/window.ui");
        let window: Window = builder.object("window").unwrap();
        let minx_entry: Entry = builder.object("minx_entry").unwrap();
        let maxx_entry: Entry = builder.object("maxx_entry").unwrap();
        let miny_entry: Entry = builder.object("miny_entry").unwrap();
        let maxy_entry: Entry = builder.object("maxy_entry").unwrap();
        let drawing_area: DrawingArea = builder.object("drawing_area").unwrap();

        minx_entry.set_text(&format!("{}", config.xrange.0));
        maxx_entry.set_text(&format!("{}", config.xrange.1));
        miny_entry.set_text(&format!("{}", config.yrange.0));
        maxy_entry.set_text(&format!("{}", config.yrange.1));

        let state = Rc::new(RefCell::new(State {
            changed: true,
            quitting: false,
            config,
            minx_entry,
            maxx_entry,
            miny_entry,
            maxy_entry,
            img: None,
        }));

        macro_rules! make_float_entry_callback {
            ( $range:ident . $idx:tt ) => {{
                let state = Rc::clone(&state);
                move |val: f64| {
                    let mut state = state.borrow_mut();
                    state.config.$range.$idx = val;
                    state.changed = true;
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
            make_reader_entry(&state_.minx_entry, make_float_entry_callback!(xrange.0));
            make_reader_entry(&state_.miny_entry, make_float_entry_callback!(yrange.0));
            make_reader_entry(&state_.maxx_entry, make_float_entry_callback!(xrange.1));
            make_reader_entry(&state_.maxy_entry, make_float_entry_callback!(yrange.1));
        }

        drawing_area.add_events(EventMask::SCROLL_MASK | EventMask::SMOOTH_SCROLL_MASK);

        drawing_area.connect_scroll_event({
            let state = Rc::clone(&state);
            move |this, event| {
                let mut state = state.borrow_mut();
                state.changed = true;
                dbg!(event.position());
                let Config {
                    xrange: (minx, maxx),
                    yrange: (miny, maxy),
                    ..
                } = &mut state.config;
            
                let xscale = *maxx - *minx;
                let yscale = *maxy - *miny;

                let (_dx, dy) = event.delta();
                dbg!(this.allocation());
                if dy < 0.0 { // "Up" = zoom in
                    let xadj = xscale / 10.0;
                    *minx += xadj;
                    *maxx -= xadj;
                    let yadj = yscale / 10.0;
                    *miny += yadj;
                    *maxy -= yadj;
                } else {
                    let xadj = xscale / 10.0;
                    *minx -= xadj;
                    *maxx += xadj;
                    let yadj = yscale / 10.0;
                    *miny -= yadj;
                    *maxy += yadj;
                }
                Inhibit(true)
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
                let img = generate(&config);
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
            move |this, _event| {
                let mut state = state.borrow_mut();
                state.changed = true;
                let allocation = this.allocation();
                state.config.width = allocation.width().try_into().expect("invalid size");
                state.config.height = allocation.height().try_into().expect("invalid size");
                dbg!();
                false
            }
        });

        drawing_area.connect_draw({
            let state = Rc::clone(&state);
            move |this, ctx| {
                dbg!(this.allocation());

                let mut state = state.borrow_mut();
                if let Some(img) = state.img.take() {
                    let width = img.width().try_into().expect("image too wide");
                    let height = img.height().try_into().expect("image too tall");
                    let data = Bytes::from_owned(img.into_vec());
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
                }
                Inhibit(true)
            }
        });

        app.add_window(&window);

        window.show_all();
    }
}
