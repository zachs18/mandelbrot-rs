#![feature(inline_const)]
#![feature(once_cell)]
#![deny(unsafe_op_in_unsafe_fn)]
use std::{rc::Rc, cell::RefCell, time::Duration, ffi::{CString, CStr}};
use gtk::{Application, prelude::{ApplicationExtManual, ApplicationExt, ObjectExt, Continue, BuilderExtManual, WidgetExtManual}, Window, traits::{GtkApplicationExt, WidgetExt, ContainerExt, GLAreaExt, ButtonExt}, GLArea, Inhibit, Builder, Button, gdk::EventMask};
use loader::AsPtr;
use watch::local::Watched;

mod util;

fn main() {
    // Register and include resources
    gtk::gio::resources_register_include!("compiled.gresource")
        .expect("Failed to register resources.");
    
    // Create a new application
    let app = Application::builder()
        .application_id("com.github.zachs18.gtk-gl")
        .build();

    // Connect to "activate" signal of `app`
    app.connect_activate(build_logic);

    // Run the application
    app.run_with_args::<&str>(&[]);
}

// fn loadfn(symbol: &str) -> *const libc::c_void {
//     static SELF: std::sync::LazyLock<loader::Library> = std::sync::LazyLock::new(|| {
//         loader::Library::this_program().expect("Failed to get dlopen handle")
//     });
//     let symbol: CString = CString::new(symbol).expect("Invalid symbol");
//     // Don't need to use sym_var_owned because SELF is static.
//     let func = SELF.sym_var::<libc::c_void>(&symbol).expect("Missing symbol");
//     func.as_ptr()
// }

fn epoxy_loadfn(symbol: &str) -> *const libc::c_void {
    static SELF: std::sync::LazyLock<loader::Library> = std::sync::LazyLock::new(|| {
        loader::Library::this_program().expect("Failed to get dlopen handle")
    });
    let symbol: CString = CString::new(format!("epoxy_{}", symbol)).expect("Invalid symbol");
    // Don't need to use sym_var_owned because SELF is static.
    let func = SELF.sym_var::<*const libc::c_void>(&symbol).expect("Missing symbol");
    // libepoxy has the symbols as function pointers, so get the symbol and dereference it.
    unsafe {*func.as_ptr()}
}

unsafe fn uniform_location(program: gl::types::GLuint, name: &CStr) -> Option<gl::types::GLint> {
    let result = unsafe { gl::GetUniformLocation(program, name.as_ptr()) };
    if result == -1 {
        None
    } else {
        Some(result)
    }
}

unsafe fn attrib_location(program: gl::types::GLuint, name: &CStr) -> Option<gl::types::GLint> {
    let result = unsafe { gl::GetAttribLocation(program, name.as_ptr()) };
    if result == -1 {
        None
    } else {
        Some(result)
    }
}

#[track_caller]
fn print_gl_error() {
    match unsafe { gl::GetError() } {
        0 => {}
        err => eprintln!("[{}] GL error # {err}", std::panic::Location::caller()),
    }
}

fn get_gl_type(value: gl::types::GLenum) -> &'static str {
    match value {
        gl::BYTE => "gl::BYTE",
        gl::FLOAT => "gl::FLOAT",
        gl::FLOAT_VEC2 => "gl::FLOAT_VEC2",
        _ => "(other)",
    }
}

fn print_program_values(program: gl::types::GLuint) {
    // Testing
    unsafe {
        let mut count = 0;
        gl::GetProgramiv(program, gl::ACTIVE_ATTRIBUTES, &mut count);
        for i in 0..count as u32 {
            let mut buf = [0u8; 256];
            let mut length = 0;
            let mut size = 0;
            let mut type_ = 0;
            gl::GetActiveAttrib(program, i, buf.len().try_into().unwrap(), &mut length, &mut size, &mut type_, buf.as_mut_ptr() as *mut i8);
            eprintln!("Attrib #{i} = {}, type = {type_:x} ({}), size = {size}", std::str::from_utf8(&buf[..length.max(0) as usize]).unwrap(), get_gl_type(type_));
        }

        gl::GetProgramiv(program, gl::ACTIVE_UNIFORMS, &mut count);
        for i in 0..count as u32 {
            let mut buf = [0u8; 256];
            let mut length = 0;
            let mut size = 0;
            let mut type_ = 0;
            gl::GetActiveUniform(program, i, buf.len().try_into().unwrap(), &mut length, &mut size, &mut type_, buf.as_mut_ptr() as *mut i8);
            eprintln!("Uniform #{i} = {}, type = {type_:x} ({}), size = {size}", std::str::from_utf8(&buf[..length.max(0) as usize]).unwrap(), get_gl_type(type_));
        }
    }
}

fn print_viewport() {
    let mut buf = [0; 4];
    unsafe {
        gl::GetIntegerv(gl::VIEWPORT, buf.as_mut_ptr());
    }
    dbg!(buf);
}

fn coordinate_convert(
    center: (f32, f32),
    zoom: f32,
    event_position: (f32, f32),
    event_window: (f32, f32),
) -> (f32, f32) {
    // Find scroll position in coordinate space
    let x = center.0 + (event_position.0 - event_window.0 / 2.0) / zoom;
    let y = center.1 + (event_position.1 - event_window.1 / 2.0) / zoom;
    (x, y)
}

fn build_logic(app: &Application) {
    let builder = Builder::from_resource("/zachs18/gtk-gl/window.ui");

    macro_rules! get_widgets {
        ( $( $(#[$($attr:tt)*])* $name:ident: $ty:ty;)* ) => {
            $(
                $(#[$($attr)*])*
                let $name: $ty = builder.object(stringify!($name)).expect(concat!("Missing widget: ", stringify!($name)));
            )*
        }
    }

    get_widgets!(
        window: Window;
        gl_area: GLArea;
        button: Button;
    );

    app.add_window(&window);

    // epoxy::load_with(loadfn);
    // gl::load_with(epoxy::get_proc_addr);
    gl::load_with(epoxy_loadfn);

    #[derive(Clone, Copy)]
    pub struct Config {
        center: (f32, f32),
        scale_factor: f32,
        hue_scale: f32,
    }

    pub struct GLState {
        vao: gl::types::GLuint,
        vertex_array_index: gl::types::GLuint,
        program: gl::types::GLuint,
        vertex_attrib: gl::types::GLint,
        center_uniform: gl::types::GLint,
        scale_uniform: gl::types::GLint,
        hue_scale_uniform: gl::types::GLint,
        // rot_uniform: gl::types::GLint,
        // ship_buffer: gl::types::GLuint,
        quad_buffer: gl::types::GLuint,
    }

    pub struct State {
        config: Rc<Watched<Config>>,
        gl_state: RefCell<Option<GLState>>,
    }

    let state: Rc<State> = Rc::new(State {
        config: Watched::new(Config { center: (0.0, 0.0), scale_factor: 1.0, hue_scale: 60.0 }),
        gl_state: RefCell::new(None),
    });

    gl_area.connect_realize({
        let state = state.clone();
        move |gl_area| {
            gl_area.make_current();
            match gl_area.error() {
                Some(_) => todo!("handle gl error"),
                None => {},
            };

            macro_rules! make_shader {
                ($var:ident $path:literal as $shader_type:expr) => {
                    let $var = unsafe {
                        let shader = gl::CreateShader($shader_type);
                        assert_ne!(shader, 0, "Failed to create shader");
                        let (count, string, length) = shader_source!($path);
                        gl::ShaderSource(shader, count, string.as_ptr(), length.as_ptr());
                        print_gl_error();
                        gl::CompileShader(shader);
                        print_gl_error();

                        let mut status = 0;
                        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
                        if status != gl::TRUE as _ {
                            eprintln!("[{}:{}] {} failed to compile", file!(), line!(), stringify!($var));
                            let mut buf = [0u8; 256];
                            let mut length = 0;
                            gl::GetShaderInfoLog(shader, buf.len().try_into().unwrap(), &mut length, buf.as_mut_ptr().cast());
                            eprintln!("Program info log: {}", std::str::from_utf8(&buf[..length.max(0) as usize]).unwrap());
                        }

                        eprintln!("TODO: error handing");
                        shader
                    };
                };
            }

            make_shader!(vertex_shader "../shaders/vertex_shader.glsl" as gl::VERTEX_SHADER);
            make_shader!(fragment_shader "../shaders/fragment_shader.glsl" as gl::FRAGMENT_SHADER);

            let program = unsafe {
                let program = gl::CreateProgram();
                assert_ne!(program, 0, "Failed to create shader program");
                print_gl_error();
                gl::AttachShader(program, vertex_shader);
                print_gl_error();
                gl::AttachShader(program, fragment_shader);
                print_gl_error();

                gl::LinkProgram(program);
                print_gl_error();
                gl::UseProgram(program);
                print_gl_error();
                {
                    let mut buf = [0u8; 256];
                    let mut length = 0;
                    gl::GetProgramInfoLog(program, buf.len().try_into().unwrap(), &mut length, buf.as_mut_ptr().cast());
                    eprintln!("Program info log: {}", std::str::from_utf8(&buf[..length.max(0) as usize]).unwrap());
                }
                eprintln!("TODO: error handing");

                program
            };

            print_program_values(program);

            // let vertex_attrib = unsafe {
            // let (vertex_attrib, center_uniform, rot_uniform, scale_uniform) = unsafe {
            let (vertex_attrib, center_uniform, scale_uniform, hue_scale_uniform) = unsafe {
                let vertex = attrib_location(program, c_str!("vertex")).expect("vertex attrib not found");
                let center = uniform_location(program, c_str!("center")).expect("center uniform not found");
                // let rot = uniform_location(program, c_str!("rot")).expect("ret uniform not found");
                let scale = uniform_location(program, c_str!("scale")).expect("scale uniform not found");
                let hue_scale = uniform_location(program, c_str!("hue_scale")).expect("hue_scale uniform not found");
                // (vertex, center, rot, scale)
                // vertex
                (vertex, center, scale, hue_scale)
            };

            let ship_verts: [f32; 8] = [
                0.0, 0.0,
                0.4, -0.4,
                0.0, 0.4,
                -0.4, -0.4,
            ];

            dbg!(gl::INVALID_ENUM, gl::INVALID_OPERATION, gl::INVALID_INDEX);

            // let ship_buffer = unsafe {
            //     let mut ship_buffer = 0;
            //     print_gl_error();
            //     gl::GenBuffers(1, &mut ship_buffer);
            //     print_gl_error();
            //     gl::BindBuffer(gl::ARRAY_BUFFER, ship_buffer);
            //     print_gl_error();
            //     gl::NamedBufferData(ship_buffer, std::mem::size_of_val(&ship_verts).try_into().unwrap(), ship_verts.as_ptr().cast(), gl::STATIC_DRAW);
            //     print_gl_error();
            //     ship_buffer
            // };

            let quad_verts: [f32; 8] = [
                -1.0, -1.0,
                -1.0, 1.0,
                1.0, -1.0,
                1.0, 1.0,
            ];

            let quad_buffer = unsafe {
                let mut quad_buffer = 0;
                print_gl_error();
                gl::GenBuffers(1, &mut quad_buffer);
                print_gl_error();
                gl::BindBuffer(gl::ARRAY_BUFFER, quad_buffer);
                print_gl_error();
                gl::NamedBufferData(quad_buffer, std::mem::size_of_val(&quad_verts).try_into().unwrap(), quad_verts.as_ptr().cast(), gl::STATIC_DRAW);
                print_gl_error();
                quad_buffer
            };

            let vao = unsafe {
                let mut vao = 0;
                print_gl_error();
                gl::GenVertexArrays(1, &mut vao);
                print_gl_error();
                gl::BindVertexArray(vao);
                print_gl_error();
                vao
            };

            let vertex_array_index = unsafe {
                let vai = 0;
                print_gl_error();
                gl::EnableVertexArrayAttrib(vao, vai);
                print_gl_error();
                vai
            };

            *state.gl_state.borrow_mut() = Some(GLState {
                program,
                vao,
                vertex_array_index,
                vertex_attrib,
                center_uniform,
                // rot_uniform,
                scale_uniform,
                hue_scale_uniform,
                // ship_buffer,
                quad_buffer,
            });
        }
    });

    gl_area.connect_unrealize({
        let state = state.clone();
        move |gl_area| {
            *state.gl_state.borrow_mut() = None;
        }
    });

    gl_area.connect_render({
        let state = state.clone();
        let buffer = RefCell::new(Vec::<f32>::new());
        move |gl_area, gl_context| -> Inhibit {
            dbg!("render");
            let gl_state = state.gl_state.borrow();
            let gl_state = match &*gl_state {
                Some(gl_state) => gl_state,
                None => todo!(),
            };
            print_gl_error();
            gl_context.make_current();
            print_gl_error();
            // print_program_values(state.program);
            print_gl_error();

            unsafe {
                gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
            }
            print_gl_error();
            dbg!("render 2");

            let Config { center, scale_factor, hue_scale } = state.config.get();

            let a = dbg!(gl_area.allocation());
            print_viewport();

            unsafe {

                gl::Uniform2f(gl_state.center_uniform, center.0, center.1);
                print_gl_error();
                gl::Uniform2f(gl_state.scale_uniform, a.width() as f32 / 400.0, a.height() as f32 / 400.0);
                print_gl_error();
                gl::Uniform1f(gl_state.hue_scale_uniform, hue_scale);
                print_gl_error();

                // gl::BindBuffer(gl::ARRAY_BUFFER, state.ship_buffer);
                gl::BindBuffer(gl::ARRAY_BUFFER, gl_state.quad_buffer);
                print_gl_error();
                gl::VertexAttribPointer(gl_state.vertex_array_index, 2, gl::FLOAT, gl::FALSE, 0, std::ptr::null());
                print_gl_error();
                // gl::DrawArrays(gl::LINE_LOOP, 0, 4);
                // print_gl_error();
                gl::DrawArrays(gl::TRIANGLE_STRIP, 0, 4);
                print_gl_error();
            }

            Inhibit(true)
        }
    });

    gl_area.add_events(
        EventMask::SCROLL_MASK
            | EventMask::SMOOTH_SCROLL_MASK
            | EventMask::BUTTON_PRESS_MASK
            | EventMask::BUTTON_RELEASE_MASK
            | EventMask::BUTTON_MOTION_MASK
            | EventMask::STRUCTURE_MASK
            | EventMask::SUBSTRUCTURE_MASK
    );

    let zoom_to = {
        let state = Rc::clone(&state);
        move |zoom_location: (f32, f32), zoom_factor: f32| {
            state.config.update(|config| {
                let Config {
                    center: (cx, cy),
                    scale_factor,
                    ..
                } = config;
                let (zoom_x, zoom_y) = zoom_location;

                // https://www.desmos.com/calculator/vvpvpvxnhi
                *cx = (*cx - zoom_x) * zoom_factor + zoom_x;
                *cy = (*cy - zoom_y) * zoom_factor + zoom_y;
                *scale_factor /= zoom_factor;
                true
            });
        }
    };

    gl_area.connect_scroll_event({
        let state = Rc::clone(&state);
        let zoom_to = zoom_to.clone();
        move |this, event| {
            let scroll_position = event.position();
            let scroll_position = (scroll_position.0 as _, scroll_position.1 as _);
            let scroll_window = this.allocation();
            let scroll_window = (scroll_window.width() as f32, scroll_window.height() as f32);

            let Config { center, scale_factor, .. } = state.config.get();

            let zoom_location =
                coordinate_convert(center, scale_factor, scroll_position, scroll_window);

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

    button.connect_clicked({
        let config = state.config.clone();
        move |_| {
            // config.update(|config| {
            //     config.hue_scale = if config.hue_scale > 30.0 {
            //         10.0
            //     } else {
            //         60.0
            //     };
            //     true
            // })
        }
    });

    gtk::glib::MainContext::default().spawn_local({
        let mut config = state.config.watch();
        let gl_area = gl_area.downgrade();
        async move {
            loop {
                match config.watch().await {
                    Ok(config) => {
                        match gl_area.upgrade() {
                            Some(gl_area) => dbg!(gl_area.queue_render(), gl_area.queue_draw()),
                            None => break,
                        };
                    },
                    Err(_) => break,
                };
            }
        }
    });

    gtk::glib::source::timeout_add_seconds(1, {
        let mut i = 0;
        move || {
            i += 1;
            dbg!(i);
            Continue(true)
        }
    });

    window.connect_configure_event({
        let state = Rc::clone(&state);
        move |this, _event| {
            dbg!("configure event");
            state.config.update(|config| {
                let allocation = this.allocation();
                // config.size = (allocation.width(), allocation.height());
                true
            });
            false
        }
    });

    window.connect_damage_event({
        let state = Rc::clone(&state);
        move |this, _event| {
            dbg!("damage event");
            state.config.update(|config| {
                let allocation = this.allocation();
                // config.size = (allocation.width(), allocation.height());
                true
            });
            false
        }
    });



    window.show_all();
}
