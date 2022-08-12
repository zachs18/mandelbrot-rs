#version 460
in vec2 pos;
uniform float hue_scale;
layout(location = 0) out vec4 FragColor;

vec3 fraction_to_hue(float fraction) {
    // fraction = (fraction % 1.0) * 1530.0;
    fraction = mod(fraction, 1.0) * 1530.0;
    if (fraction <= 255.0) return vec3(255.0, fraction, 0) / 255.0;
    if (fraction <= 510.0) return vec3(512.0 - fraction, 255.0, 0) / 255.0;
    if (fraction <= 765.0) return vec3(0, 255.0, fraction - 510.0) / 255.0;
    if (fraction <= 1020.0) return vec3(0, 1020.0 - fraction, 255.0) / 255.0;
    if (fraction <= 1275.0) return vec3(fraction - 1020.0, 0, 255.0) / 255.0;
    return vec3(255.0, 0, 1360.0 - fraction) / 255.0;
}
// fn fraction_to_hue(x: f64) -> Rgb<u8> {
//     let x = x % 1.0;
//     let x = (x * 1530.0) as u32;
//     #[allow(overlapping_range_endpoints)]
//     match x {
//         0..=255 => Rgb([255, x as u8, 0]),
//         255..=510 => Rgb([(510 - x) as u8, 255, 0]),
//         510..=765 => Rgb([0, 255, (x - 510) as u8]),
//         765..=1020 => Rgb([0, (1020 - x) as u8, 255]),
//         1020..=1275 => Rgb([(x - 1020) as u8, 0, 255]),
//         1275..=1530 => Rgb([255, 0, (1530 - x) as u8]),
//         _ => unreachable!("x <= 1530"),
//     }
// }

void main() {
    // FragColor = vec4(1);
    // FragColor = gl_FragCoord / 400;
    vec2 z = vec2(0);
    vec2 c = pos;
    int i = 0;
    for (i = 0; i < 10000; ++i) {
        z = vec2(
            z.x * z.x - z.y * z.y + c.x,
            z.y * z.x + z.x * z.y + c.y
        );

        if (length(z) > 4.0) break;
    }
    // float hue = (i / 10000.0) * 1536.0;
    // FragColor = vec4(vec3(i / 10000.0), 1.0);
    if (i == 10000) FragColor = vec4(0, 0, 0, 1);
    else FragColor = vec4(fraction_to_hue(i / 10000.0 * hue_scale), 1.0);
}
