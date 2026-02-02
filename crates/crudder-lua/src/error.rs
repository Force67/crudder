//! Error types for the Lua recipe system.

use thiserror::Error;

/// Errors that can occur in the Lua recipe system.
#[derive(Debug, Error)]
pub enum RecipeError {
    /// Recipe not found.
    #[error("recipe not found: {0}")]
    NotFound(String),

    /// Invalid recipe format.
    #[error("invalid recipe format: {0}")]
    InvalidFormat(String),

    /// Lua execution error.
    #[error("lua error: {0}")]
    Lua(#[from] mlua::Error),

    /// Invalid path (absolute or contains ..).
    #[error("invalid path: {0}")]
    InvalidPath(String),

    /// IO error.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

/// Result type for recipe operations.
pub type Result<T> = std::result::Result<T, RecipeError>;
