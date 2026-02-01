//! TypeScript code generation passes.
//!
//! This module provides a pass-based architecture for generating TypeScript code.
//! The base pass generates pure TypeScript interfaces, and feature passes can be
//! added to enhance the output with framework-specific features like Zod validation,
//! Express handlers, or a client SDK.
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

pub mod base;
pub mod passes;

pub use base::TypeScriptBasePass;
pub use base::{primitive_to_typescript, to_camel_case, to_snake_case, type_ref_to_typescript};

use crate::pass::PassManager;

/// Creates a PassManager with the full TypeScript stack (types + Zod + Express + client).
///
/// This is a convenience function that creates a pass manager pre-configured
/// with all TypeScript passes for generating a complete TypeScript package.
pub fn full_stack() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(TypeScriptBasePass);
    pm.add(passes::ZodPass);
    pm.add(passes::ExpressPass);
    pm.add(passes::ClientPass);
    pm
}

/// Creates a PassManager with just the base TypeScript types.
///
/// This generates only the pure TypeScript interfaces with no dependencies.
pub fn types_only() -> PassManager {
    let mut pm = PassManager::new();
    pm.add(TypeScriptBasePass);
    pm
}
