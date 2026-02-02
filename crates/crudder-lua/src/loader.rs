//! Recipe discovery and loading.

use std::path::PathBuf;

use mlua::{Lua, Table};

use crate::error::{RecipeError, Result};

/// Embedded built-in recipes.
pub mod builtin {
    pub const RUST_AXUM: &str = include_str!("../../../recipes/rust-axum.lua");
    pub const RUST_BUILD: &str = include_str!("../../../recipes/rust-build.lua");
    pub const RUST_TRAIT: &str = include_str!("../../../recipes/rust-trait.lua");
    pub const TYPESCRIPT_REACT: &str = include_str!("../../../recipes/typescript-react.lua");
    pub const TYPESCRIPT_TRAIT: &str = include_str!("../../../recipes/typescript-trait.lua");
    pub const EXAMPLE_SIMPLE: &str = include_str!("../../../recipes/example-simple.lua");
}

/// Information about an available recipe.
#[derive(Debug, Clone)]
pub struct RecipeInfo {
    /// The recipe name.
    pub name: String,
    /// The recipe description.
    pub description: String,
    /// Whether this is a built-in recipe.
    pub builtin: bool,
    /// Path to the recipe file (if not built-in).
    pub path: Option<PathBuf>,
}

/// A loaded recipe ready for execution.
pub struct LoadedRecipe {
    pub info: RecipeInfo,
    pub source: String,
}

/// Recipe loader that discovers and loads recipes.
pub struct RecipeLoader {
    /// Project-local recipes directory.
    project_dir: Option<PathBuf>,
    /// User-global recipes directory.
    user_dir: Option<PathBuf>,
}

impl RecipeLoader {
    /// Creates a new recipe loader.
    pub fn new() -> Self {
        let user_dir = dirs::config_dir().map(|p| p.join("crudder").join("recipes"));

        Self {
            project_dir: None,
            user_dir,
        }
    }

    /// Sets the project directory for local recipes.
    pub fn with_project_dir(mut self, dir: PathBuf) -> Self {
        self.project_dir = Some(dir.join(".crudder").join("recipes"));
        self
    }

    /// Lists all available recipes.
    pub fn list_recipes(&self) -> Vec<RecipeInfo> {
        let mut recipes = Vec::new();

        // Built-in recipes
        recipes.push(RecipeInfo {
            name: "rust-axum".to_string(),
            description: "Rust + Axum HTTP server".to_string(),
            builtin: true,
            path: None,
        });
        recipes.push(RecipeInfo {
            name: "rust-build".to_string(),
            description: "Rust code for build.rs (single file output)".to_string(),
            builtin: true,
            path: None,
        });
        recipes.push(RecipeInfo {
            name: "rust-trait".to_string(),
            description: "Rust service trait (user implements handlers)".to_string(),
            builtin: true,
            path: None,
        });
        recipes.push(RecipeInfo {
            name: "typescript-react".to_string(),
            description: "TypeScript + React client".to_string(),
            builtin: true,
            path: None,
        });
        recipes.push(RecipeInfo {
            name: "typescript-trait".to_string(),
            description: "TypeScript service interface (user implements handlers)".to_string(),
            builtin: true,
            path: None,
        });
        recipes.push(RecipeInfo {
            name: "example-simple".to_string(),
            description: "Simple example demonstrating new UX features".to_string(),
            builtin: true,
            path: None,
        });

        // Project-local recipes
        if let Some(ref dir) = self.project_dir {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    if let Some(info) = self.recipe_info_from_file(&entry.path()) {
                        recipes.push(info);
                    }
                }
            }
        }

        // User-global recipes
        if let Some(ref dir) = self.user_dir {
            if let Ok(entries) = std::fs::read_dir(dir) {
                for entry in entries.flatten() {
                    if let Some(info) = self.recipe_info_from_file(&entry.path()) {
                        // Don't add if already exists (project takes precedence)
                        if !recipes.iter().any(|r| r.name == info.name) {
                            recipes.push(info);
                        }
                    }
                }
            }
        }

        recipes
    }

    /// Loads a recipe by name.
    pub fn load(&self, name: &str) -> Result<LoadedRecipe> {
        // Check built-in recipes first
        match name {
            "rust-axum" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "rust-axum".to_string(),
                        description: "Rust + Axum HTTP server".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::RUST_AXUM.to_string(),
                });
            }
            "rust-build" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "rust-build".to_string(),
                        description: "Rust code for build.rs (single file output)".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::RUST_BUILD.to_string(),
                });
            }
            "rust-trait" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "rust-trait".to_string(),
                        description: "Rust service trait (user implements handlers)".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::RUST_TRAIT.to_string(),
                });
            }
            "typescript-react" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "typescript-react".to_string(),
                        description: "TypeScript + React client".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::TYPESCRIPT_REACT.to_string(),
                });
            }
            "typescript-trait" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "typescript-trait".to_string(),
                        description: "TypeScript service interface (user implements handlers)".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::TYPESCRIPT_TRAIT.to_string(),
                });
            }
            "example-simple" => {
                return Ok(LoadedRecipe {
                    info: RecipeInfo {
                        name: "example-simple".to_string(),
                        description: "Simple example demonstrating new UX features".to_string(),
                        builtin: true,
                        path: None,
                    },
                    source: builtin::EXAMPLE_SIMPLE.to_string(),
                });
            }
            _ => {}
        }

        // Check project-local recipes
        if let Some(ref dir) = self.project_dir {
            let path = dir.join(format!("{}.lua", name));
            if path.exists() {
                return self.load_from_file(&path);
            }
        }

        // Check user-global recipes
        if let Some(ref dir) = self.user_dir {
            let path = dir.join(format!("{}.lua", name));
            if path.exists() {
                return self.load_from_file(&path);
            }
        }

        Err(RecipeError::NotFound(name.to_string()))
    }

    /// Loads a recipe from a file path.
    pub fn load_from_file(&self, path: &PathBuf) -> Result<LoadedRecipe> {
        let source = std::fs::read_to_string(path)?;
        let info = self
            .recipe_info_from_source(&source, Some(path.clone()))?;
        Ok(LoadedRecipe { info, source })
    }

    /// Extracts recipe info from a file path.
    fn recipe_info_from_file(&self, path: &PathBuf) -> Option<RecipeInfo> {
        if path.extension().map(|e| e.to_str()) != Some(Some("lua")) {
            return None;
        }

        let source = std::fs::read_to_string(path).ok()?;
        self.recipe_info_from_source(&source, Some(path.clone())).ok()
    }

    /// Extracts recipe info from source code.
    fn recipe_info_from_source(&self, source: &str, path: Option<PathBuf>) -> Result<RecipeInfo> {
        let lua = Lua::new();
        let recipe: Table = lua.load(source).eval().map_err(|e| {
            RecipeError::InvalidFormat(format!("failed to evaluate recipe: {}", e))
        })?;

        let name: String = recipe.get("name").map_err(|_| {
            RecipeError::InvalidFormat("recipe missing 'name' field".to_string())
        })?;

        let description: String = recipe
            .get("description")
            .unwrap_or_else(|_| "No description".to_string());

        Ok(RecipeInfo {
            name,
            description,
            builtin: false,
            path,
        })
    }
}

impl Default for RecipeLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_builtin_recipes() {
        let loader = RecipeLoader::new();
        let recipes = loader.list_recipes();

        assert!(recipes.iter().any(|r| r.name == "rust-axum" && r.builtin));
        assert!(recipes
            .iter()
            .any(|r| r.name == "typescript-react" && r.builtin));
    }

    #[test]
    fn test_load_builtin_recipe() {
        let loader = RecipeLoader::new();

        let recipe = loader.load("rust-axum").unwrap();
        assert_eq!(recipe.info.name, "rust-axum");
        assert!(recipe.info.builtin);
        assert!(!recipe.source.is_empty());

        let recipe = loader.load("typescript-react").unwrap();
        assert_eq!(recipe.info.name, "typescript-react");
    }

    #[test]
    fn test_load_nonexistent_recipe() {
        let loader = RecipeLoader::new();
        let result = loader.load("nonexistent");
        assert!(matches!(result, Err(RecipeError::NotFound(_))));
    }
}
