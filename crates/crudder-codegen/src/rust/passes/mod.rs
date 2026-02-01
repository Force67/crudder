//! Rust feature passes.
//!
//! This module contains passes that add framework-specific features
//! to the generated Rust code.

pub mod axum;
pub mod serde;
pub mod sqlx;

pub use axum::AxumPass;
pub use serde::SerdePass;
pub use sqlx::SqlxPass;
