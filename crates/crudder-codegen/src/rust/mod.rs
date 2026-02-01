//! Rust code generation passes.
//!
//! This module provides a pass-based architecture for generating Rust code.
//! The base pass generates pure Rust structs with minimal derives, and feature
//! passes can be added to enhance the output with framework-specific features
//! like Serde serialization, Axum handlers, or SQLx database access.
//!
//! # Example
//!
//! ```ignore
//! use crudder_codegen::pass::PassManager;
//! use crudder_codegen::rust::{RustBasePass, passes::{SerdePass, AxumPass}};
//!
//! let mut pm = PassManager::new();
//! pm.add(RustBasePass);
//! pm.add(SerdePass);
//! pm.add(AxumPass);
//!
//! let files = pm.run(&schema)?;
//! ```

pub mod base;
pub mod passes;

pub use base::RustBasePass;
pub use base::{primitive_to_rust, to_snake_case, type_ref_to_rust};

use proc_macro2::TokenStream;

use crate::pass::PassManager;
use crate::CodegenError;

pub(crate) fn format_rust(tokens: TokenStream) -> Result<String, CodegenError> {
    let file = syn::parse2::<syn::File>(tokens)
        .map_err(|e| CodegenError::Custom(format!("generated invalid Rust code: {e}")))?;
    Ok(prettyplease::unparse(&file))
}

/// Creates a PassManager with the full Rust + Axum stack (types + serde + axum).
///
/// This is a convenience function that creates a pass manager pre-configured
/// with passes for generating a complete Axum web service with stub handlers.
pub fn axum_stack() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(RustBasePass);
    pm.add(passes::SerdePass);
    pm.add(passes::AxumPass);
    pm
}

/// Creates a PassManager with the full SQLx stack (types + serde + axum + sqlx).
///
/// This is a convenience function that creates a pass manager pre-configured
/// with all passes for generating a complete database-backed web service.
pub fn sqlx_postgres_stack() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(RustBasePass);
    pm.add(passes::SerdePass);
    pm.add(passes::AxumPass);
    pm.add(passes::SqlxPass::postgres());
    pm
}

/// Creates a PassManager with the full SQLx SQLite stack.
pub fn sqlx_sqlite_stack() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(RustBasePass);
    pm.add(passes::SerdePass);
    pm.add(passes::AxumPass);
    pm.add(passes::SqlxPass::sqlite());
    pm
}

/// Creates a PassManager with just the base Rust types.
///
/// This generates only the pure Rust structs with Debug and Clone derives.
pub fn types_only() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(RustBasePass);
    pm
}

/// Creates a PassManager with Rust types and Serde serialization.
pub fn with_serde() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(RustBasePass);
    pm.add(passes::SerdePass);
    pm
}
