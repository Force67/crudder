//! Code generation backends for the Crudder DSL.
//!
//! This crate provides a pass-based code generation architecture where:
//! 1. A base pass generates pure, library-agnostic code
//! 2. Feature passes (Zod, Express, Axum, SQLx) are independent modules that enhance the output
//! 3. Passes can be composed in any order via the PassManager
//!
//! # Example
//!
//! ```ignore
//! use crudder_codegen::pass::PassManager;
//! use crudder_codegen::typescript::{TypeScriptBasePass, passes::{ZodPass, ExpressPass}};
//!
//! let mut pm = PassManager::new();
//! pm.add(TypeScriptBasePass);
//! pm.add(ZodPass);
//! pm.add(ExpressPass);
//!
//! let files = pm.run(&schema)?;
//! ```

use std::path::PathBuf;

use crudder_ast::Schema;
use thiserror::Error;

// Pass-based modules
pub mod pass;
pub mod protobuf;
pub mod rust;
pub mod typescript;

// Re-export pass types
pub use pass::{GenerationContext, Pass, PassManager};

/// Errors that can occur during code generation.
#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("unknown type reference: {0}")]
    UnknownType(String),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Custom(String),
}

/// A generated file with its path and content.
#[derive(Debug, Clone)]
pub struct GeneratedFile {
    /// Relative path for the generated file.
    pub path: PathBuf,
    /// Content of the generated file.
    pub content: String,
}

impl GeneratedFile {
    /// Creates a new generated file.
    pub fn new(path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            content: content.into(),
        }
    }
}

/// Result of code generation.
#[derive(Debug, Clone, Default)]
pub struct GeneratedFiles {
    /// The generated files.
    pub files: Vec<GeneratedFile>,
}

impl GeneratedFiles {
    /// Creates a new empty result.
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a file to the result.
    pub fn add(&mut self, file: GeneratedFile) {
        self.files.push(file);
    }

    /// Writes all files to the given output directory.
    pub fn write_to(&self, output_dir: &std::path::Path) -> Result<(), std::io::Error> {
        for file in &self.files {
            validate_relative_path(&file.path)?;
            let path = output_dir.join(&file.path);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&path, &file.content)?;
        }
        Ok(())
    }
}

fn validate_relative_path(path: &std::path::Path) -> Result<(), std::io::Error> {
    use std::path::Component;

    if path.is_absolute() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("generated file path must be relative: {}", path.display()),
        ));
    }

    for component in path.components() {
        match component {
            Component::Normal(_) => {}
            // Disallow `..`, `.`, prefixes, and root dirs to prevent escaping `output_dir`.
            Component::ParentDir | Component::CurDir | Component::Prefix(_) | Component::RootDir => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("generated file path must be a normal relative path: {}", path.display()),
                ));
            }
        }
    }

    Ok(())
}

/// Trait for code generators.
pub trait CodeGenerator {
    /// Generates code from a schema.
    fn generate(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError>;

    /// Returns the name of this generator.
    fn name(&self) -> &'static str;
}

/// Available code generation targets (legacy API).
///
/// For Rust and TypeScript, use [`PassManager`] with the appropriate passes instead.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// Protobuf definitions.
    Protobuf,
}

impl std::str::FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "protobuf" | "proto" => Ok(Target::Protobuf),
            _ => Err(format!("unknown target: {s}")),
        }
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::Protobuf => write!(f, "protobuf"),
        }
    }
}

/// Creates a code generator for the given target.
///
/// For Rust and TypeScript code generation, use [`PassManager`] with the
/// appropriate passes instead. See the crate documentation for examples.
pub fn generator_for(target: Target) -> Box<dyn CodeGenerator> {
    match target {
        Target::Protobuf => Box::new(protobuf::ProtobufGenerator::new()),
    }
}
