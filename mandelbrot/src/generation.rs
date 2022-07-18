use std::{ops::ControlFlow, sync::Arc};

use f128::f128;
use image::{Rgb, RgbImage};
use num_complex::Complex;
use num_traits::{Num, NumCast, Float};

#[derive(Debug, Clone, Copy)]
pub enum Precision {
    Single,
    Double,
    Quad,
}

#[derive(Clone)]
pub enum Fractal {
    Mandelbrot,
    BurningShip,
    #[cfg(feature = "custom_fractals")]
    Custom {
        single: Arc<dyn Sync + Send + Fn(Complex<f32>) -> Box<dyn FnMut(Complex<f32>) -> Complex<f32>>>,
        double: Arc<dyn Sync + Send + Fn(Complex<f64>) -> Box<dyn FnMut(Complex<f64>) -> Complex<f64>>>,
        quad: Arc<dyn Sync + Send + Fn(Complex<f128>) -> Box<dyn FnMut(Complex<f128>) -> Complex<f128>>>,
    }
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

pub fn generate_with<T, F, G>(config: Config, iteration_fn_maker: F) -> RgbImage
    where
        T: Clone + Num + PartialOrd + NumCast,
        F: Clone + Fn(Complex<T>) -> G,
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

    let mut img = image::RgbImage::new(width, height);
    let limit = T::from(4.0).unwrap();
    let zero = T::from(0.0).unwrap();
    for y in 0..height {
        let im: T = cy.clone() + T::from(y as f64 - height as f64 / 2.0).unwrap() / zoom.clone();
        for x in 0..width {
            let re: T = cx.clone() + T::from(x as f64 - width as f64 / 2.0).unwrap() / zoom.clone();
            let c = Complex { re, im: im.clone() };
            let mut f = iteration_fn_maker(c);
            let iterations =
                (0..max_iterations).try_fold(Complex::new(zero.clone(), zero.clone()), |val, iteration| {
                    let val = f(val);
                    if val.norm_sqr() > limit {
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

pub fn generate_mandelbrot_precision<T>(config: Config) -> RgbImage
    where
        T: Clone + Num + PartialOrd + NumCast
{
    generate_with(config, |c: Complex<T>| {
        move |z: Complex<T>| {
            z.clone() * z + c.clone()
        }
    })
}
pub fn generate_mandelbrot(config: Config) -> RgbImage {
    match config.precision {
        Precision::Single => generate_mandelbrot_precision::<f32>(config),
        Precision::Double => generate_mandelbrot_precision::<f64>(config),
        Precision::Quad => generate_mandelbrot_precision::<f128>(config),
    }
}

pub fn generate_burning_ship_precision<T>(config: Config) -> RgbImage
where
    T: Clone + Num + PartialOrd + NumCast + Float
{
    generate_with(config, |c: Complex<T>| {
        move |z: Complex<T>| {
            let z = Complex { re: z.re.abs(), im: z.im.abs() };
            z * z + c
        }
    })
}
pub fn generate_burning_ship(config: Config) -> RgbImage {
    match config.precision {
        Precision::Single => generate_burning_ship_precision::<f32>(config),
        Precision::Double => generate_burning_ship_precision::<f64>(config),
        Precision::Quad => generate_burning_ship_precision::<f128>(config),
    }
}

pub fn generate(config: Config) -> RgbImage {
    match &config.fractal {
        Fractal::Mandelbrot => generate_mandelbrot(config),
        Fractal::BurningShip => generate_burning_ship(config),
        #[cfg(feature = "custom_fractals")]
        Fractal::Custom { single, double, quad } => match config.precision {
            Precision::Single => {
                let single = single.clone();
                generate_with(config, move |val| single(val))
            }
            Precision::Double => {
                let double = double.clone();
                generate_with(config, move |val| double(val))
            }
            Precision::Quad => {
                let quad = quad.clone();
                generate_with(config, move |val| quad(val))
            }
        },
        
    }
}
