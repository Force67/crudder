//! Lua recipe system for Crudder code generation.
//!
//! This crate provides a Lua-based recipe system that replaces Rust-based code generation.
//! Recipes are Lua scripts that receive a schema and emit generated files.
//!
//! # Example
//!
//! ```ignore
//! use crudder_lua::{RecipeRunner, RecipeLoader};
//! use crudder_ast::Schema;
//!
//! let schema: Schema = /* parse schema */;
//! let loader = RecipeLoader::new();
//! let recipe = loader.load("rust-axum")?;
//!
//! let mut runner = RecipeRunner::new();
//! runner.set_option("sqlx", "postgres");
//! let files = runner.run(&schema, &recipe)?;
//!
//! for file in files {
//!     println!("{}: {} bytes", file.path, file.content.len());
//! }
//! ```

pub mod bindings;
pub mod context;
pub mod error;
pub mod helpers;
pub mod loader;

pub use context::{EmittedFile, RecipeContext};
pub use error::{RecipeError, Result};
pub use loader::{LoadedRecipe, RecipeInfo, RecipeLoader};

use std::collections::HashMap;
use std::path::PathBuf;

use crudder_ast::Schema;
use mlua::{Function, Lua, Table};

use bindings::LuaSchema;

/// Runs Lua recipes against schemas.
pub struct RecipeRunner {
    options: HashMap<String, String>,
}

impl RecipeRunner {
    /// Creates a new recipe runner.
    pub fn new() -> Self {
        Self {
            options: HashMap::new(),
        }
    }

    /// Sets a recipe option.
    pub fn set_option(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.options.insert(key.into(), value.into());
    }

    /// Sets multiple options from an iterator.
    pub fn set_options(&mut self, options: impl IntoIterator<Item = (String, String)>) {
        self.options.extend(options);
    }

    /// Runs a recipe against a schema.
    pub fn run(&self, schema: &Schema, recipe: &LoadedRecipe) -> Result<Vec<EmittedFile>> {
        let lua = Lua::new();

        helpers::register_helpers(&lua)?;

        let recipe_table: Table = lua.load(&recipe.source).eval()?;

        let generate: Function = recipe_table.get("generate").map_err(|_| {
            RecipeError::InvalidFormat("recipe missing 'generate' function".to_string())
        })?;

        let ctx = RecipeContext::with_options(self.options.clone());
        let lua_schema = LuaSchema(schema.clone());

        lua.scope(|scope| {
            let ctx_ud = scope.create_userdata(ctx.clone())?;
            let schema_ud = scope.create_userdata(lua_schema)?;

            generate.call::<()>((schema_ud, ctx_ud))?;
            Ok(())
        })?;

        Ok(ctx.files())
    }
}

impl Default for RecipeRunner {
    fn default() -> Self {
        Self::new()
    }
}

/// High-level API: Generate files from a schema using a recipe.
pub fn generate(
    schema: &Schema,
    recipe_name: &str,
    options: HashMap<String, String>,
) -> Result<Vec<EmittedFile>> {
    let loader = RecipeLoader::new();
    let recipe = loader.load(recipe_name)?;

    let mut runner = RecipeRunner::new();
    runner.set_options(options);
    runner.run(schema, &recipe)
}

/// High-level API: Generate files and write them to a directory.
pub fn generate_to_dir(
    schema: &Schema,
    recipe_name: &str,
    options: HashMap<String, String>,
    output_dir: &PathBuf,
) -> Result<Vec<EmittedFile>> {
    let files = generate(schema, recipe_name, options)?;

    std::fs::create_dir_all(output_dir)?;

    for file in &files {
        let path = output_dir.join(&file.path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&path, &file.content)?;
    }

    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::*;

    fn create_test_schema() -> Schema {
        Schema {
            dtos: vec![
                Dto {
                    name: "Todo".to_string(),
                    fields: vec![
                        Field {
                            name: "id".to_string(),
                            ty: TypeRef::Primitive(PrimitiveType::Uuid),
                            annotations: vec![Annotation::new("primary"), Annotation::new("auto")],
                            index: None,
                            span: None,
                        },
                        Field {
                            name: "title".to_string(),
                            ty: TypeRef::Primitive(PrimitiveType::String),
                            annotations: vec![],
                            index: None,
                            span: None,
                        },
                    ],
                    annotations: vec![Annotation::with_args("table", vec!["todos".to_string()])],
                    span: None,
                },
                Dto {
                    name: "CreateTodoRequest".to_string(),
                    fields: vec![Field {
                        name: "title".to_string(),
                        ty: TypeRef::Primitive(PrimitiveType::String),
                        annotations: vec![],
                        index: None,
                        span: None,
                    }],
                    annotations: vec![],
                    span: None,
                },
            ],
            services: vec![Service {
                name: "TodoService".to_string(),
                methods: vec![Method {
                    name: "CreateTodo".to_string(),
                    input: "CreateTodoRequest".to_string(),
                    output: Some("Todo".to_string()),
                    annotations: vec![Annotation::with_args(
                        "rest",
                        vec!["POST".to_string(), "/todos".to_string()],
                    )],
                    span: None,
                }],
                annotations: vec![Annotation::with_args(
                    "storage",
                    vec!["postgres".to_string()],
                )],
                span: None,
            }],
        }
    }

    #[test]
    fn test_simple_recipe() {
        let schema = create_test_schema();

        let recipe = LoadedRecipe {
            info: RecipeInfo {
                name: "test".to_string(),
                description: "Test recipe".to_string(),
                builtin: false,
                path: None,
            },
            source: r#"
                return {
                    name = "test",
                    description = "Test recipe",
                    generate = function(schema, ctx)
                        local content = "// Generated\n"
                        for _, dto in ipairs(schema.dtos) do
                            content = content .. "struct " .. dto.name .. " {}\n"
                        end
                        ctx:emit_file("types.rs", content)
                    end
                }
            "#
            .to_string(),
        };

        let runner = RecipeRunner::new();
        let files = runner.run(&schema, &recipe).unwrap();

        assert_eq!(files.len(), 1);
        assert_eq!(files[0].path, "types.rs");
        assert!(files[0].content.contains("struct Todo"));
        assert!(files[0].content.contains("struct CreateTodoRequest"));
    }

    #[test]
    fn test_recipe_with_options() {
        let schema = create_test_schema();

        let recipe = LoadedRecipe {
            info: RecipeInfo {
                name: "test".to_string(),
                description: "Test recipe".to_string(),
                builtin: false,
                path: None,
            },
            source: r#"
                return {
                    name = "test",
                    description = "Test recipe",
                    generate = function(schema, ctx)
                        local db = ctx:option("database") or "none"
                        ctx:emit_file("config.txt", "database=" .. db)
                    end
                }
            "#
            .to_string(),
        };

        let mut runner = RecipeRunner::new();
        runner.set_option("database", "postgres");
        let files = runner.run(&schema, &recipe).unwrap();

        assert_eq!(files.len(), 1);
        assert_eq!(files[0].content, "database=postgres");
    }

    #[test]
    fn test_recipe_with_helpers() {
        let schema = create_test_schema();

        let recipe = LoadedRecipe {
            info: RecipeInfo {
                name: "test".to_string(),
                description: "Test recipe".to_string(),
                builtin: false,
                path: None,
            },
            source: r#"
                return {
                    name = "test",
                    description = "Test recipe",
                    generate = function(schema, ctx)
                        local snake = helpers.to_snake_case("TodoService")
                        ctx:emit_file("test.txt", snake)
                    end
                }
            "#
            .to_string(),
        };

        let runner = RecipeRunner::new();
        let files = runner.run(&schema, &recipe).unwrap();

        assert_eq!(files[0].content, "todo_service");
    }
}
