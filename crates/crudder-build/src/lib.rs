//! Build-time code generation for Crudder schemas.
//!
//! This crate is intended to be used in a `build.rs` file to generate Rust code
//! from `.crudder` schema files, similar to how `tonic-build` works for protobuf.
//!
//! # Example
//!
//! In your `Cargo.toml`:
//! ```toml
//! [build-dependencies]
//! crudder-build = "0.1"
//! ```
//!
//! In your `build.rs`:
//! ```ignore
//! fn main() {
//!     crudder_build::Config::new()
//!         .sqlx("postgres")
//!         .compile(&["schema.crudder"])
//!         .unwrap();
//! }
//! ```
//!
//! In your `src/lib.rs`:
//! ```ignore
//! // Include the generated code
//! pub mod schema {
//!     include!(concat!(env!("OUT_DIR"), "/schema.rs"));
//! }
//! ```

use std::collections::HashMap;
use std::path::Path;

/// Configuration for code generation.
#[derive(Default)]
pub struct Config {
    recipe: String,
    options: HashMap<String, String>,
}

impl Config {
    /// Creates a new configuration with default settings.
    /// Uses the "rust-trait" recipe which generates a service trait for custom logic.
    pub fn new() -> Self {
        Self {
            recipe: "rust-trait".to_string(),
            options: HashMap::new(),
        }
    }

    /// Sets the recipe to use for code generation.
    pub fn recipe(mut self, recipe: &str) -> Self {
        self.recipe = recipe.to_string();
        self
    }

    /// Configures SQLx support (postgres, sqlite, or none).
    pub fn sqlx(mut self, dialect: &str) -> Self {
        self.options.insert("sqlx".to_string(), dialect.to_string());
        self
    }

    /// Sets a custom option for the recipe.
    pub fn option(mut self, key: &str, value: &str) -> Self {
        self.options.insert(key.to_string(), value.to_string());
        self
    }

    /// Compiles the given schema files.
    pub fn compile<P: AsRef<Path>>(self, schemas: &[P]) -> Result<(), Box<dyn std::error::Error>> {
        let out_dir = std::env::var("OUT_DIR")?;

        // Load the recipe
        let loader = crudder_lua::RecipeLoader::new();
        let recipe = loader.load(&self.recipe)?;

        for schema_path in schemas {
            let schema_path = schema_path.as_ref();

            // Tell cargo to rerun if the schema changes
            println!("cargo:rerun-if-changed={}", schema_path.display());

            // Parse the schema
            let source = std::fs::read_to_string(schema_path)?;
            let schema = crudder_parser::parse(&source)
                .map_err(|e| format!("failed to parse schema: {:?}", e))?;

            // Generate code
            let mut runner = crudder_lua::RecipeRunner::new();
            for (key, value) in &self.options {
                runner.set_option(key, value);
            }
            let files = runner.run(&schema, &recipe)?;

            // Determine the module name from the schema filename
            let stem = schema_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("schema");

            // Write the generated file(s)
            // The rust-build recipe generates a single file; others may generate multiple
            for file in &files {
                if file.path.ends_with(".rs") {
                    // Use the schema stem as the filename
                    let out_path = Path::new(&out_dir).join(format!("{}.rs", stem));
                    std::fs::write(&out_path, &file.content)?;
                    break; // Only write the first .rs file
                }
            }
        }

        Ok(())
    }
}

/// Convenience function to compile schemas with default settings.
pub fn compile<P: AsRef<Path>>(schemas: &[P]) -> Result<(), Box<dyn std::error::Error>> {
    Config::new().compile(schemas)
}
