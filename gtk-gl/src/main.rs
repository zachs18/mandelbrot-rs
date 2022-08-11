#![feature(inline_const)]
#![feature(once_cell)]
#![deny(unsafe_op_in_unsafe_fn)]
use std::{rc::Rc, cell::RefCell, time::Duration, ffi::{CString, CStr}};
use gtk::{Application, prelude::{ApplicationExtManual, ApplicationExt, ObjectExt, Continue, BuilderExtManual}, Window, traits::{GtkApplicationExt, WidgetExt, ContainerExt, GLAreaExt, ButtonExt}, GLArea, Inhibit, Builder, Button};
use loader::AsPtr;

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

fn loadfn(symbol: &str) -> *const libc::c_void {
    static SELF: std::sync::LazyLock<loader::Library> = std::sync::LazyLock::new(|| {
        loader::Library::this_program().expect("Failed to get dlopen handle")
    });
    let symbol: CString = CString::new(symbol).expect("Invalid symbol");
    // Don't need to use sym_var_owned because SELF is static.
    let func = SELF.sym_var::<libc::c_void>(&symbol).expect("Missing symbol");
    func.as_ptr()
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

    epoxy::load_with(loadfn);
    gl::load_with(epoxy::get_proc_addr);

    pub struct State {
        pos: (f32, f32),
        angle: f32,
        angle_delta: f32,
        vao: gl::types::GLuint,
        vertex_array_index: gl::types::GLuint,
        program: gl::types::GLuint,
        vertex_attrib: gl::types::GLint,
        pos_uniform: gl::types::GLint,
        scale_uniform: gl::types::GLint,
        rot_uniform: gl::types::GLint,
        ship_buffer: gl::types::GLuint,
    }

    let state: Rc<RefCell<Option<State>>> = Rc::new(RefCell::new(None));

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
                eprintln!("TODO: error handing");

                program
            };

            print_program_values(program);

            let (vertex_attrib, pos_uniform, rot_uniform, scale_uniform) = unsafe {
                let vertex = attrib_location(program, c_str!("vertex")).expect("vertex attrib not found");
                let pos = uniform_location(program, c_str!("pos")).expect("pos uniform not found");
                let rot = uniform_location(program, c_str!("rot")).expect("ret uniform not found");
                let scale = uniform_location(program, c_str!("scale")).expect("scale uniform not found");
                (vertex, pos, rot, scale)
            };

            let ship_verts: [f32; 8] = [
                0.0, 0.0,
                0.4, -0.4,
                0.0, 0.4,
                -0.4, -0.4,
            ];

            dbg!(gl::INVALID_ENUM, gl::INVALID_OPERATION, gl::INVALID_INDEX);

            let ship_buffer = unsafe {
                let mut ship_buffer = 0;
                print_gl_error();
                gl::GenBuffers(1, &mut ship_buffer);
                print_gl_error();
                gl::BindBuffer(gl::ARRAY_BUFFER, ship_buffer);
                print_gl_error();
                gl::NamedBufferData(ship_buffer, std::mem::size_of_val(&ship_verts).try_into().unwrap(), ship_verts.as_ptr().cast(), gl::STATIC_DRAW);
                print_gl_error();
                ship_buffer
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

            *state.borrow_mut() = Some(State {
                pos: (0.0, 0.0),
                angle: 0.0,
                angle_delta: 0.01,
                program,
                vao,
                vertex_array_index,
                vertex_attrib,
                pos_uniform,
                rot_uniform,
                scale_uniform,
                ship_buffer,
            });
        }
    });

    gl_area.connect_unrealize({
        let state = state.clone();
        move |gl_area| {
            *state.borrow_mut() = None;
        }
    });

    gl_area.connect_render({
        let state = state.clone();
        let buffer = RefCell::new(Vec::<f32>::new());
        move |gl_area, gl_context| -> Inhibit {
            let state = state.borrow();
            let state = match &*state {
                Some(state) => state,
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

            let a = gl_area.allocation();

            unsafe {

                gl::Uniform1f(state.rot_uniform, state.angle);
                print_gl_error();
                gl::Uniform2f(state.pos_uniform, state.pos.0, state.pos.1);
                print_gl_error();
                gl::Uniform2f(state.scale_uniform, a.width() as f32 / 400.0, a.height() as f32 / 400.0);
                print_gl_error();


                gl::BindBuffer(gl::ARRAY_BUFFER, state.ship_buffer);
                print_gl_error();
                gl::VertexAttribPointer(state.vertex_array_index, 2, gl::FLOAT, gl::FALSE, 0, std::ptr::null());
                print_gl_error();
                gl::DrawArrays(gl::LINE_LOOP, 0, 4);
                print_gl_error();
            }

            Inhibit(true)
        }
    });

    button.connect_clicked({
        let state = state.clone();
        move |_| {
            match &mut *state.borrow_mut() {
                Some(state) => state.angle_delta *= -1.0,
                None => {},
            }
        }
    });

    gtk::glib::source::timeout_add_local(Duration::from_secs(1) / 30, {
        let state = state.clone();
        let gl_area = gl_area.downgrade();
        move || {
            match gl_area.upgrade() {
                Some(gl_area) => {
                    match &mut *state.borrow_mut() {
                        Some(state) => state.angle += state.angle_delta,
                        None => todo!(),
                    }
                    gl_area.queue_draw();
                    Continue(true)
                },
                None => Continue(false),
            }
        }
    });



    window.show_all();
}
