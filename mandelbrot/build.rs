use std::env;
use std::process::Command;

fn main() {
    {
        // gio::compile_resources(
        //     "resources",
        //     "resources/resources.gresource.xml",
        //     "compiled.gresource",
        // );
        // inlined from gio-0.15.12/src/resource.rs
        // to reduce compilation times; otherwise we'd have to compile all
        // of gio and its dependencies for just that function
        let source_dir = "resources";
        let gresource = "resources/resources.gresource.xml";
        let target = "compiled.gresource";
        let out_dir = env::var("OUT_DIR").unwrap();

        let status = Command::new("glib-compile-resources")
            .arg("--sourcedir")
            .arg(source_dir)
            .arg("--target")
            .arg(&format!("{}/{}", out_dir, target))
            .arg(gresource)
            .status()
            .unwrap();

        assert!(
            status.success(),
            "glib-compile-resources failed with exit status {}",
            status
        );

        println!("cargo:rerun-if-changed={}", gresource);
        let output = Command::new("glib-compile-resources")
            .arg("--sourcedir")
            .arg(source_dir)
            .arg("--generate-dependencies")
            .arg(gresource)
            .output()
            .unwrap()
            .stdout;
        let output = String::from_utf8(output).unwrap();
        for dep in output.split_whitespace() {
            println!("cargo:rerun-if-changed={}", dep);
        }
    };
}
