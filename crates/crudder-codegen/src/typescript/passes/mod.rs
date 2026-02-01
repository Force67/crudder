//! TypeScript feature passes.
//!
//! This module contains passes that add framework-specific features
//! to the generated TypeScript code.

pub mod client;
pub mod express;
pub mod zod;

pub use client::ClientPass;
pub use express::ExpressPass;
pub use zod::ZodPass;
