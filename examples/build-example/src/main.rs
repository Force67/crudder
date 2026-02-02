//! Example showing trait-based code generation with crudder-build.
//!
//! The generated code provides a trait that you implement with custom logic.
//! This is similar to how tonic-build works for gRPC services.

use axum::async_trait;
use axum::http::StatusCode;
use sqlx::PgPool;

// Include the generated code from OUT_DIR
mod todo {
    include!(concat!(env!("OUT_DIR"), "/todo.rs"));
}

pub use todo::*;

// =============================================================================
// Your custom service implementation
// =============================================================================

/// Your service struct - holds any state you need (database pool, config, etc.)
#[derive(Clone)]
pub struct MyTodoService {
    pool: PgPool,
}

impl MyTodoService {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
}

/// Implement the generated trait with your custom logic
#[async_trait]
impl TodoService for MyTodoService {
    async fn list_todos(&self) -> Result<TodoList, StatusCode> {
        let todos = sqlx::query_as::<_, Todo>(
            "SELECT * FROM todos ORDER BY created_at DESC"
        )
        .fetch_all(&self.pool)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

        Ok(TodoList { todos })
    }

    async fn create_todo(&self, request: CreateTodoRequest) -> Result<Todo, StatusCode> {
        let result = sqlx::query_as::<_, Todo>(
            "INSERT INTO todos (title, completed, user_id) VALUES ($1, $2, $3) RETURNING *"
        )
        .bind(&request.title)
        .bind(false)
        .bind(uuid::Uuid::nil()) // TODO: get from auth context
        .fetch_one(&self.pool)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

        Ok(result)
    }

    async fn get_todo(&self, id: String) -> Result<Todo, StatusCode> {
        let id = id.parse::<uuid::Uuid>().map_err(|_| StatusCode::BAD_REQUEST)?;

        let result = sqlx::query_as::<_, Todo>(
            "SELECT * FROM todos WHERE id = $1"
        )
        .bind(id)
        .fetch_optional(&self.pool)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

        result.ok_or(StatusCode::NOT_FOUND)
    }

    async fn update_todo(&self, id: String, request: UpdateTodoRequest) -> Result<Todo, StatusCode> {
        let id = id.parse::<uuid::Uuid>().map_err(|_| StatusCode::BAD_REQUEST)?;

        let result = sqlx::query_as::<_, Todo>(
            "UPDATE todos SET title = COALESCE($1, title), completed = COALESCE($2, completed) WHERE id = $3 RETURNING *"
        )
        .bind(&request.title)
        .bind(&request.completed)
        .bind(id)
        .fetch_optional(&self.pool)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

        result.ok_or(StatusCode::NOT_FOUND)
    }

    async fn delete_todo(&self, id: String) -> StatusCode {
        let id = match id.parse::<uuid::Uuid>() {
            Ok(id) => id,
            Err(_) => return StatusCode::BAD_REQUEST,
        };

        let result = match sqlx::query("DELETE FROM todos WHERE id = $1")
            .bind(id)
            .execute(&self.pool)
            .await
        {
            Ok(r) => r,
            Err(_) => return StatusCode::INTERNAL_SERVER_ERROR,
        };

        if result.rows_affected() > 0 {
            StatusCode::NO_CONTENT
        } else {
            StatusCode::NOT_FOUND
        }
    }
}

// =============================================================================
// Main
// =============================================================================

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let database_url = std::env::var("DATABASE_URL")
        .unwrap_or_else(|_| "postgres://postgres:postgres@localhost:5432/crudder_demo".to_string());

    println!("Connecting to database: {}", database_url);

    let pool = sqlx::postgres::PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await?;

    println!("Connected!");

    let service = MyTodoService::new(pool);

    let app = todo_service_router(service);

    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], 3000));
    println!("Server listening on http://{}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
