use std::ops::ControlFlow;

use image::{Rgb, RgbImage};
use num_complex::Complex;

#[derive(Debug, Clone, Copy)]
pub struct Config {
    pub max_iterations: u32,
    pub center: (f64, f64),
    pub zoom: f64,
    pub hue_scale: f64,
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

pub fn generate(config: &Config, width: i32, height: i32) -> RgbImage {
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
