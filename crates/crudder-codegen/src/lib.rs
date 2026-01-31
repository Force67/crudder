//! Code generation backends for the Crudder DSL.

use std::path::PathBuf;

use crudder_ast::Schema;
use thiserror::Error;

pub mod protobuf;
pub mod rust;
pub mod sqlx;
pub mod typescript;

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
            let path = output_dir.join(&file.path);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&path, &file.content)?;
        }
        Ok(())
    }
}

/// Trait for code generators.
pub trait CodeGenerator {
    /// Generates code from a schema.
    fn generate(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError>;

    /// Returns the name of this generator.
    fn name(&self) -> &'static str;
}

/// Available code generation targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// Basic Rust with stub handlers.
    Rust,
    /// TypeScript types and client.
    TypeScript,
    /// Protobuf definitions.
    Protobuf,
    /// Rust with SQLx Postgres (full CRUD implementation).
    SqlxPostgres,
    /// Rust with SQLx SQLite (full CRUD implementation).
    SqlxSqlite,
}

impl std::str::FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "rust" => Ok(Target::Rust),
            "typescript" | "ts" => Ok(Target::TypeScript),
            "protobuf" | "proto" => Ok(Target::Protobuf),
            "sqlx-postgres" | "postgres" | "pg" => Ok(Target::SqlxPostgres),
            "sqlx-sqlite" | "sqlite" => Ok(Target::SqlxSqlite),
            _ => Err(format!("unknown target: {s}")),
        }
    }
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::Rust => write!(f, "rust"),
            Target::TypeScript => write!(f, "typescript"),
            Target::Protobuf => write!(f, "protobuf"),
            Target::SqlxPostgres => write!(f, "sqlx-postgres"),
            Target::SqlxSqlite => write!(f, "sqlx-sqlite"),
        }
    }
}

/// Creates a code generator for the given target.
pub fn generator_for(target: Target) -> Box<dyn CodeGenerator> {
    match target {
        Target::Rust => Box::new(rust::RustGenerator::new()),
        Target::TypeScript => Box::new(typescript::TypeScriptGenerator::new()),
        Target::Protobuf => Box::new(protobuf::ProtobufGenerator::new()),
        Target::SqlxPostgres => Box::new(sqlx::SqlxGenerator::postgres()),
        Target::SqlxSqlite => Box::new(sqlx::SqlxGenerator::sqlite()),
    }
}
