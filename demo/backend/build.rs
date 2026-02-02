use std::env;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let schema_path = Path::new("../../examples/todo.crudder");

    // Re-run if schema changes
    println!("cargo:rerun-if-changed={}", schema_path.display());

    let source = std::fs::read_to_string(schema_path).expect("failed to read schema");
    let schema = crudder_parser::parse(&source).expect("failed to parse schema");

    let generator = crudder_codegen::sqlx::SqlxGenerator::postgres();
    let files = crudder_codegen::CodeGenerator::generate(&generator, &schema)
        .expect("failed to generate code");

    for file in &files.files {
        let path = Path::new(&out_dir).join(&file.path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }

        // Strip module-level items that don't work with include!()
        let content = file.content
            .lines()
            .filter(|line| {
                // Strip inner doc comments (//!)
                !line.starts_with("//!")
                // Strip crate::types imports (handled by wrapper)
                && !line.starts_with("use crate::types")
            })
            .collect::<Vec<_>>()
            .join("\n");

        std::fs::write(&path, content).unwrap();
    }
}
