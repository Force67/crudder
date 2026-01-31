//! Todo backend - generated from examples/todo.crudder

pub mod types {
    include!(concat!(env!("OUT_DIR"), "/src/types.rs"));
}

pub mod auth {
    include!(concat!(env!("OUT_DIR"), "/src/auth.rs"));
}

pub mod todo_service {
    #[allow(unused_imports)]
    use crate::types::*;

    include!(concat!(env!("OUT_DIR"), "/src/todo_service.rs"));
}

pub use todo_service::{todo_service_router, AppState};

use axum::Router;

pub fn router() -> Router<AppState> {
    Router::new().merge(todo_service_router())
}

/// Migration SQL (embedded from generated code)
pub const MIGRATION_SQL: &str = include_str!(concat!(env!("OUT_DIR"), "/migrations/001_create_todos.sql"));
