use std::{num::NonZeroU32, ops::ControlFlow, sync::Arc};

use image::{ImageBuffer, Rgb, RgbImage};
use num_complex::Complex;
use num_traits::{Float, Num, NumCast};

#[derive(Debug, Clone, Copy)]
pub enum Precision {
    Single,
    Double,
    #[cfg(feature = "f128")]
    Quad,
}

type FunctionGenerator<T> =
    dyn Sync + Send + Fn(Complex<T>) -> Box<dyn FnMut(Complex<T>) -> Complex<T>>;

#[derive(Clone)]
pub enum Fractal {
    Mandelbrot,
    BurningShip,
    #[cfg(feature = "custom_fractals")]
    Custom {
        single: Arc<FunctionGenerator<f32>>,
        double: Arc<FunctionGenerator<f64>>,
        #[cfg(feature = "f128")]
        quad: Arc<FunctionGenerator<f128>>,
    },
}

impl std::fmt::Debug for Fractal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mandelbrot => write!(f, "Mandelbrot"),
            Self::BurningShip => write!(f, "BurningShip"),
            #[cfg(feature = "custom_fractals")]
            Self::Custom { .. } => write!(f, "Custom"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    pub size: (i32, i32),
    pub max_iterations: u32,
    pub center: (f64, f64),
    pub zoom: f64,
    pub hue_scale: f64,
    pub precision: Precision,
    pub fractal: Fractal,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            size: (512, 512),
            max_iterations: 1000,
            center: (-0.75, 0.0),
            zoom: 192.0,
            hue_scale: 60.0,
            precision: Precision::Single,
            fractal: Fractal::Mandelbrot,
        }
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

fn split_chunks(
    image: &mut RgbImage,
    split_count: NonZeroU32,
) -> (u32, Vec<ImageBuffer<Rgb<u8>, &mut [u8]>>) {
    let width = image.width();
    let height = image.height();
    let image = std::ops::DerefMut::deref_mut(image);
    let common_height = height / split_count;
    let last_height = height - common_height * (split_count.get() - 1);
    if common_height == 0 || split_count.get() == 1 {
        return (
            height,
            vec![ImageBuffer::from_raw(width, height, image).unwrap()],
        );
    }

    let common_size = common_height as usize * width as usize * 3;
    let last_size = last_height as usize * width as usize * 3;

    let mut chunks =
        Vec::with_capacity(split_count.get().try_into().expect("u32 -> usize overflow"));

    (0..split_count.get()).fold(image, |image, chunk| {
        if chunk < split_count.get() - 1 {
            // Not the last chunk
            let (chunk, image) = image.split_at_mut(common_size);
            let chunk = ImageBuffer::from_raw(width, common_height, chunk).unwrap();
            chunks.push(chunk);
            image
        } else {
            // the last chunk
            assert_eq!(image.len(), last_size, "Image was wrong size");
            let chunk = ImageBuffer::from_raw(width, last_height, image).unwrap();
            chunks.push(chunk);
            &mut []
        }
    });

    (common_height, chunks)
}

pub fn generate_with<T, F, G>(config: Config, iteration_fn_maker: F) -> RgbImage
where
    T: Clone + Send + Sync + Num + PartialOrd + NumCast,
    F: Clone + Send + Sync + Fn(Complex<T>) -> G,
    G: FnMut(Complex<T>) -> Complex<T>,
{
    let Config {
        size: (width, height),
        max_iterations,
        center: (cx, cy),
        zoom,
        hue_scale,
        ..
    } = config;
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();
    let zoom = T::from(zoom).unwrap();
    let cy = T::from(cy).unwrap();
    let cx = T::from(cx).unwrap();

    #[cfg(feature = "multithread_generation")]
    if num_cpus::get() > 1 {
        let mut img = image::RgbImage::new(width, height);
        let limit = T::from(4.0).unwrap();
        let zero = T::from(0.0).unwrap();

        let process_chunk = move |mut chunk: ImageBuffer<Rgb<u8>, &mut [u8]>,
                                  idx: usize,
                                  common_height: u32| {
            let start_row = idx as u32 * common_height;
            for y in 0..chunk.height() {
                let im: T = cy.clone()
                    + T::from((y + start_row) as f64 - height as f64 / 2.0).unwrap() / zoom.clone();
                for x in 0..width {
                    let re: T =
                        cx.clone() + T::from(x as f64 - width as f64 / 2.0).unwrap() / zoom.clone();
                    let c = Complex { re, im: im.clone() };
                    let mut f = iteration_fn_maker(c);
                    let iterations = (0..max_iterations).try_fold(
                        Complex::new(zero.clone(), zero.clone()),
                        |val, iteration| {
                            let val = f(val);
                            if val.norm_sqr() > limit {
                                ControlFlow::Break(iteration)
                            } else {
                                ControlFlow::Continue(val)
                            }
                        },
                    );

                    *chunk.get_pixel_mut(x, y) = match iterations {
                        ControlFlow::Break(it) => {
                            fraction_to_hue((it as f64 / max_iterations as f64) * hue_scale)
                        }
                        ControlFlow::Continue(_) => image::Rgb([0, 0, 0]),
                    };
                }
            }
        };

        // Subtract one, to leave one core for the GUI thread.
        let threads = NonZeroU32::new(
            (num_cpus::get().max(2) - 1)
                .try_into()
                .expect("u32 -> usize overflow"),
        )
        .unwrap();

        let (common_height, chunks) = split_chunks(&mut img, threads);

        std::thread::scope(|scope| {
            let mut chunks = chunks.into_iter().enumerate();
            let (_zero, this_thread_chunk) = chunks.next().unwrap();
            for (idx, chunk) in chunks {
                let process_chunk = &process_chunk;

                static TODO_ONCE: std::sync::Once = std::sync::Once::new();
                TODO_ONCE.call_once(|| {
                    eprintln!("TODO: use something like a scoped threadpool, to prevent repeatedly spawning and tearing down so many threads.");
                });

                scope.spawn(move || {
                    process_chunk(chunk, idx, common_height);
                });
            }
            process_chunk(this_thread_chunk, 0, common_height);
        });

        return img;
    }

    // Used if multithread_generation is disabled, or if multithread_generation
    // is enabled, but only one CPU core is available.
    let mut img = image::RgbImage::new(width, height);
    let limit = T::from(4.0).unwrap();
    let zero = T::from(0.0).unwrap();
    for y in 0..height {
        let im: T = cy.clone() + T::from(y as f64 - height as f64 / 2.0).unwrap() / zoom.clone();
        for x in 0..width {
            let re: T = cx.clone() + T::from(x as f64 - width as f64 / 2.0).unwrap() / zoom.clone();
            let c = Complex { re, im: im.clone() };
            let mut f = iteration_fn_maker(c);
            let iterations = (0..max_iterations).try_fold(
                Complex::new(zero.clone(), zero.clone()),
                |val, iteration| {
                    let val = f(val);
                    if val.norm_sqr() > limit {
                        ControlFlow::Break(iteration)
                    } else {
                        ControlFlow::Continue(val)
                    }
                },
            );

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

pub fn generate_mandelbrot_precision<T>(config: Config) -> RgbImage
where
    T: Clone + Send + Sync + Num + PartialOrd + NumCast,
{
    generate_with(config, |c: Complex<T>| {
        move |z: Complex<T>| z.clone() * z + c.clone()
    })
}
pub fn generate_mandelbrot(config: Config) -> RgbImage {
    match config.precision {
        Precision::Single => generate_mandelbrot_precision::<f32>(config),
        Precision::Double => generate_mandelbrot_precision::<f64>(config),
        #[cfg(feature = "f128")]
        Precision::Quad => generate_mandelbrot_precision::<f128>(config),
    }
}

pub fn generate_burning_ship_precision<T>(config: Config) -> RgbImage
where
    T: Clone + Send + Sync + Num + PartialOrd + NumCast + Float,
{
    generate_with(config, |c: Complex<T>| {
        move |z: Complex<T>| {
            let z = Complex {
                re: z.re.abs(),
                im: z.im.abs(),
            };
            z * z + c
        }
    })
}
pub fn generate_burning_ship(config: Config) -> RgbImage {
    match config.precision {
        Precision::Single => generate_burning_ship_precision::<f32>(config),
        Precision::Double => generate_burning_ship_precision::<f64>(config),
        #[cfg(feature = "f128")]
        Precision::Quad => generate_burning_ship_precision::<f128>(config),
    }
}

pub fn generate(config: Config) -> RgbImage {
    match &config.fractal {
        Fractal::Mandelbrot => generate_mandelbrot(config),
        Fractal::BurningShip => generate_burning_ship(config),
        #[cfg(feature = "custom_fractals")]
        Fractal::Custom {
            single,
            double,
            #[cfg(feature = "f128")]
            quad,
        } => match config.precision {
            Precision::Single => {
                let single = single.clone();
                generate_with(config, move |val| single(val))
            }
            Precision::Double => {
                let double = double.clone();
                generate_with(config, move |val| double(val))
            }
            #[cfg(feature = "f128")]
            Precision::Quad => {
                let quad = quad.clone();
                generate_with(config, move |val| quad(val))
            }
        },
    }
}
