//! RecipeContext for Lua recipes to emit files and access options.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use mlua::{UserData, UserDataMethods};

use crate::error::RecipeError;

/// A file emitted by a recipe.
#[derive(Debug, Clone)]
pub struct EmittedFile {
    pub path: String,
    pub content: String,
}

/// Internal state for the recipe context.
#[derive(Debug, Default)]
pub struct ContextState {
    pub files: HashMap<String, String>,
    pub options: HashMap<String, String>,
    pub logs: Vec<String>,
    pub warnings: Vec<String>,
}

/// Recipe context exposed to Lua.
#[derive(Clone)]
pub struct RecipeContext {
    state: Rc<RefCell<ContextState>>,
}

impl RecipeContext {
    /// Creates a new recipe context.
    pub fn new() -> Self {
        Self {
            state: Rc::new(RefCell::new(ContextState::default())),
        }
    }

    /// Creates a context with options.
    pub fn with_options(options: HashMap<String, String>) -> Self {
        let ctx = Self::new();
        ctx.state.borrow_mut().options = options;
        ctx
    }

    /// Returns all emitted files.
    pub fn files(&self) -> Vec<EmittedFile> {
        self.state
            .borrow()
            .files
            .iter()
            .map(|(path, content)| EmittedFile {
                path: path.clone(),
                content: content.clone(),
            })
            .collect()
    }

    /// Returns all log messages.
    pub fn logs(&self) -> Vec<String> {
        self.state.borrow().logs.clone()
    }

    /// Returns all warnings.
    pub fn warnings(&self) -> Vec<String> {
        self.state.borrow().warnings.clone()
    }

    /// Validates a path for security.
    fn validate_path(path: &str) -> std::result::Result<(), RecipeError> {
        // Reject absolute paths
        if path.starts_with('/') || path.starts_with('\\') {
            return Err(RecipeError::InvalidPath(format!(
                "absolute paths not allowed: {}",
                path
            )));
        }

        // Reject Windows absolute paths (C:, D:, etc.)
        if path.len() >= 2 && path.chars().nth(1) == Some(':') {
            return Err(RecipeError::InvalidPath(format!(
                "absolute paths not allowed: {}",
                path
            )));
        }

        // Reject path traversal
        for component in path.split(['/', '\\']) {
            if component == ".." {
                return Err(RecipeError::InvalidPath(format!(
                    "path traversal not allowed: {}",
                    path
                )));
            }
        }

        Ok(())
    }
}

impl Default for RecipeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl UserData for RecipeContext {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        // emit_file(path, content) - emit a file
        methods.add_method("emit_file", |_, this, (path, content): (String, String)| {
            Self::validate_path(&path).map_err(|e| mlua::Error::external(e))?;
            this.state.borrow_mut().files.insert(path, content);
            Ok(())
        });

        // has_file(path) - check if file exists
        methods.add_method("has_file", |_, this, path: String| {
            Ok(this.state.borrow().files.contains_key(&path))
        });

        // get_file(path) - get file content
        methods.add_method("get_file", |_, this, path: String| {
            Ok(this.state.borrow().files.get(&path).cloned())
        });

        // option(key) - get recipe option
        methods.add_method("option", |_, this, key: String| {
            Ok(this.state.borrow().options.get(&key).cloned())
        });

        // log(msg) - log message
        methods.add_method("log", |_, this, msg: String| {
            this.state.borrow_mut().logs.push(msg);
            Ok(())
        });

        // warn(msg) - log warning
        methods.add_method("warn", |_, this, msg: String| {
            this.state.borrow_mut().warnings.push(msg);
            Ok(())
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mlua::Lua;

    #[test]
    fn test_emit_file() {
        let ctx = RecipeContext::new();
        let lua = Lua::new();
        lua.scope(|scope| {
            let ctx_ud = scope.create_userdata(ctx.clone()).unwrap();
            lua.globals().set("ctx", ctx_ud).unwrap();

            lua.load(r#"ctx:emit_file("src/foo.rs", "fn main() {}")"#)
                .exec()
                .unwrap();

            Ok(())
        })
        .unwrap();

        let files = ctx.files();
        assert_eq!(files.len(), 1);
        assert_eq!(files[0].path, "src/foo.rs");
        assert_eq!(files[0].content, "fn main() {}");
    }

    #[test]
    fn test_path_validation() {
        assert!(RecipeContext::validate_path("src/foo.rs").is_ok());
        assert!(RecipeContext::validate_path("foo.rs").is_ok());
        assert!(RecipeContext::validate_path("/etc/passwd").is_err());
        assert!(RecipeContext::validate_path("../../../etc/passwd").is_err());
        assert!(RecipeContext::validate_path("foo/../bar").is_err());
        assert!(RecipeContext::validate_path("C:\\Windows\\System32").is_err());
    }

    #[test]
    fn test_options() {
        let mut options = HashMap::new();
        options.insert("sqlx".to_string(), "postgres".to_string());
        let ctx = RecipeContext::with_options(options);

        let lua = Lua::new();
        lua.scope(|scope| {
            let ctx_ud = scope.create_userdata(ctx.clone()).unwrap();
            lua.globals().set("ctx", ctx_ud).unwrap();

            let result: Option<String> = lua.load(r#"ctx:option("sqlx")"#).eval().unwrap();
            assert_eq!(result, Some("postgres".to_string()));

            let result: Option<String> = lua.load(r#"ctx:option("nonexistent")"#).eval().unwrap();
            assert_eq!(result, None);

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_has_file_and_get_file() {
        let ctx = RecipeContext::new();
        let lua = Lua::new();

        lua.scope(|scope| {
            let ctx_ud = scope.create_userdata(ctx.clone()).unwrap();
            lua.globals().set("ctx", ctx_ud).unwrap();

            lua.load(r#"ctx:emit_file("test.txt", "hello")"#)
                .exec()
                .unwrap();

            let has: bool = lua.load(r#"ctx:has_file("test.txt")"#).eval().unwrap();
            assert!(has);

            let has: bool = lua.load(r#"ctx:has_file("other.txt")"#).eval().unwrap();
            assert!(!has);

            let content: Option<String> = lua.load(r#"ctx:get_file("test.txt")"#).eval().unwrap();
            assert_eq!(content, Some("hello".to_string()));

            Ok(())
        })
        .unwrap();
    }
}
