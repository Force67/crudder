//! Lua helper functions for string manipulation, case conversion, and code generation.

use crate::bindings::LuaTypeRef;
use mlua::{Lua, Result, UserData, UserDataMethods};
use std::cell::RefCell;
use std::rc::Rc;

/// Registers the helpers module in Lua.
pub fn register_helpers(lua: &Lua) -> Result<()> {
    let helpers = lua.create_table()?;

    // Case conversion
    helpers.set("to_snake_case", lua.create_function(to_snake_case)?)?;
    helpers.set("to_pascal_case", lua.create_function(to_pascal_case)?)?;
    helpers.set("to_camel_case", lua.create_function(to_camel_case)?)?;
    helpers.set("to_kebab_case", lua.create_function(to_kebab_case)?)?;

    // String manipulation
    helpers.set("indent", lua.create_function(indent)?)?;
    helpers.set("dedent", lua.create_function(dedent)?)?;
    helpers.set("trim", lua.create_function(trim)?)?;

    // Template interpolation
    helpers.set("template", lua.create_function(template)?)?;

    // Type mappers
    helpers.set("rust_type", lua.create_function(rust_type)?)?;
    helpers.set("ts_type", lua.create_function(ts_type)?)?;
    helpers.set("sql_type", lua.create_function(sql_type)?)?;

    // Line builder
    helpers.set("lines", lua.create_function(create_lines)?)?;

    lua.globals().set("helpers", helpers)?;
    Ok(())
}

// =============================================================================
// Case Conversion
// =============================================================================

fn to_snake_case(_lua: &Lua, s: String) -> Result<String> {
    Ok(convert_to_snake_case(&s))
}

fn to_pascal_case(_lua: &Lua, s: String) -> Result<String> {
    Ok(convert_to_pascal_case(&s))
}

fn to_camel_case(_lua: &Lua, s: String) -> Result<String> {
    let pascal = convert_to_pascal_case(&s);
    if pascal.is_empty() {
        return Ok(pascal);
    }
    let mut chars = pascal.chars();
    let first = chars.next().unwrap().to_lowercase();
    Ok(first.chain(chars).collect())
}

fn to_kebab_case(_lua: &Lua, s: String) -> Result<String> {
    Ok(convert_to_snake_case(&s).replace('_', "-"))
}

fn convert_to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;

    for c in s.chars() {
        if c == '-' || c == '_' || c == ' ' {
            result.push('_');
            prev_lower = false;
        } else if c.is_uppercase() {
            if prev_lower {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
            prev_lower = false;
        } else {
            result.push(c);
            prev_lower = c.is_lowercase();
        }
    }

    result
}

fn convert_to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c == '-' || c == '_' || c == ' ' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }

    result
}

// =============================================================================
// String Manipulation
// =============================================================================

fn indent(_lua: &Lua, (s, n): (String, usize)) -> Result<String> {
    let prefix = " ".repeat(n);
    Ok(s.lines()
        .map(|line| {
            if line.is_empty() {
                line.to_string()
            } else {
                format!("{}{}", prefix, line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n"))
}

fn dedent(_lua: &Lua, s: String) -> Result<String> {
    let lines: Vec<&str> = s.lines().collect();
    if lines.is_empty() {
        return Ok(s);
    }

    let min_indent = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start().len())
        .min()
        .unwrap_or(0);

    Ok(lines
        .into_iter()
        .map(|line| {
            if line.len() >= min_indent {
                &line[min_indent..]
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n"))
}

fn trim(_lua: &Lua, s: String) -> Result<String> {
    Ok(s.trim().to_string())
}

// =============================================================================
// Template Interpolation
// =============================================================================

/// Template with ${var} interpolation.
/// Usage: helpers.template("Hello ${name}!", {name = "World"})
fn template(_lua: &Lua, (tmpl, vars): (String, mlua::Table)) -> Result<String> {
    let mut result = tmpl;

    for pair in vars.pairs::<String, mlua::Value>() {
        let (key, value) = pair?;
        let value_str = lua_value_to_string(&value);
        let pattern = format!("${{{}}}", key);
        result = result.replace(&pattern, &value_str);
    }

    Ok(result)
}

fn lua_value_to_string(value: &mlua::Value) -> String {
    match value {
        mlua::Value::String(s) => s.to_str().map(|s| s.to_string()).unwrap_or_default(),
        mlua::Value::Integer(i) => i.to_string(),
        mlua::Value::Number(n) => n.to_string(),
        mlua::Value::Boolean(b) => b.to_string(),
        mlua::Value::Nil => String::new(),
        _ => format!("{:?}", value),
    }
}

// =============================================================================
// Type Mappers
// =============================================================================

/// Maps a Crudder type to Rust type.
/// Usage: helpers.rust_type(field.ty) or helpers.rust_type(field.ty, {qualified = true})
fn rust_type(_lua: &Lua, (ty, opts): (LuaTypeRef, Option<mlua::Table>)) -> Result<String> {
    let qualified = opts
        .and_then(|t| t.get::<bool>("qualified").ok())
        .unwrap_or(false);

    Ok(type_to_rust(&ty.0, !qualified))
}

/// Maps a Crudder type to TypeScript type.
/// Usage: helpers.ts_type(field.ty)
fn ts_type(_lua: &Lua, ty: LuaTypeRef) -> Result<String> {
    Ok(type_to_typescript(&ty.0))
}

/// Maps a Crudder type to SQL type.
/// Usage: helpers.sql_type(field.ty) or helpers.sql_type(field.ty, "sqlite")
fn sql_type(_lua: &Lua, (ty, dialect): (LuaTypeRef, Option<String>)) -> Result<String> {
    let dialect = dialect.as_deref().unwrap_or("postgres");
    Ok(type_to_sql(&ty.0, dialect))
}

fn type_to_rust(ty: &crudder_ast::TypeRef, short: bool) -> String {
    use crudder_ast::TypeRef;

    match ty {
        TypeRef::Primitive(p) => primitive_to_rust(p, short),
        TypeRef::Array(inner) => format!("Vec<{}>", type_to_rust(inner, short)),
        TypeRef::Optional(inner) => format!("Option<{}>", type_to_rust(inner, short)),
        TypeRef::Named(name) => name.clone(),
    }
}

fn primitive_to_rust(p: &crudder_ast::PrimitiveType, short: bool) -> String {
    use crudder_ast::PrimitiveType;

    if short {
        match p {
            PrimitiveType::String => "String",
            PrimitiveType::Int => "i64",
            PrimitiveType::Float => "f64",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Uuid => "Uuid",
            PrimitiveType::Cuid2 => "String",
            PrimitiveType::Timestamp => "DateTime<Utc>",
            PrimitiveType::Bytes => "Vec<u8>",
        }
    } else {
        match p {
            PrimitiveType::String => "String",
            PrimitiveType::Int => "i64",
            PrimitiveType::Float => "f64",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Uuid => "uuid::Uuid",
            PrimitiveType::Cuid2 => "String",
            PrimitiveType::Timestamp => "chrono::DateTime<chrono::Utc>",
            PrimitiveType::Bytes => "Vec<u8>",
        }
    }
    .to_string()
}

fn type_to_typescript(ty: &crudder_ast::TypeRef) -> String {
    use crudder_ast::{PrimitiveType, TypeRef};

    match ty {
        TypeRef::Primitive(p) => match p {
            PrimitiveType::String | PrimitiveType::Uuid | PrimitiveType::Cuid2 => "string",
            PrimitiveType::Int | PrimitiveType::Float => "number",
            PrimitiveType::Bool => "boolean",
            PrimitiveType::Timestamp => "string", // ISO date string
            PrimitiveType::Bytes => "Uint8Array",
        }
        .to_string(),
        TypeRef::Array(inner) => format!("{}[]", type_to_typescript(inner)),
        TypeRef::Optional(inner) => format!("{} | null", type_to_typescript(inner)),
        TypeRef::Named(name) => name.clone(),
    }
}

fn type_to_sql(ty: &crudder_ast::TypeRef, dialect: &str) -> String {
    use crudder_ast::{PrimitiveType, TypeRef};

    match ty {
        TypeRef::Primitive(p) => {
            if dialect == "postgres" {
                match p {
                    PrimitiveType::String => "TEXT",
                    PrimitiveType::Int => "BIGINT",
                    PrimitiveType::Float => "DOUBLE PRECISION",
                    PrimitiveType::Bool => "BOOLEAN",
                    PrimitiveType::Uuid => "UUID",
                    PrimitiveType::Cuid2 => "TEXT",
                    PrimitiveType::Timestamp => "TIMESTAMPTZ",
                    PrimitiveType::Bytes => "BYTEA",
                }
            } else {
                // SQLite
                match p {
                    PrimitiveType::String
                    | PrimitiveType::Uuid
                    | PrimitiveType::Cuid2
                    | PrimitiveType::Timestamp => "TEXT",
                    PrimitiveType::Int | PrimitiveType::Bool => "INTEGER",
                    PrimitiveType::Float => "REAL",
                    PrimitiveType::Bytes => "BLOB",
                }
            }
            .to_string()
        }
        TypeRef::Optional(inner) => type_to_sql(inner, dialect),
        TypeRef::Array(_) | TypeRef::Named(_) => {
            if dialect == "postgres" {
                "JSONB".to_string()
            } else {
                "TEXT".to_string()
            }
        }
    }
}

// =============================================================================
// Line Builder
// =============================================================================

/// Creates a new line builder for fluent code generation.
fn create_lines(_lua: &Lua, (): ()) -> Result<LuaLineBuilder> {
    Ok(LuaLineBuilder::new())
}

/// Internal shared state for the line builder.
struct LineBuilderState {
    lines: Vec<String>,
    indent_level: usize,
    indent_str: String,
}

/// A fluent line builder for generating code.
#[derive(Clone)]
pub struct LuaLineBuilder {
    state: Rc<RefCell<LineBuilderState>>,
}

impl LuaLineBuilder {
    fn new() -> Self {
        Self {
            state: Rc::new(RefCell::new(LineBuilderState {
                lines: Vec::new(),
                indent_level: 0,
                indent_str: "    ".to_string(), // 4 spaces default
            })),
        }
    }

    fn current_indent(&self) -> String {
        let state = self.state.borrow();
        state.indent_str.repeat(state.indent_level)
    }

    fn add_line(&self, line: String) {
        let indent = self.current_indent();
        self.state.borrow_mut().lines.push(format!("{}{}", indent, line));
    }
}

impl UserData for LuaLineBuilder {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        // Add a single line
        methods.add_method("add", |_, this, line: String| {
            this.add_line(line);
            Ok(this.clone())
        });

        // Add a line without any indentation
        methods.add_method("add_raw", |_, this, line: String| {
            this.state.borrow_mut().lines.push(line);
            Ok(this.clone())
        });

        // Add an empty line
        methods.add_method("blank", |_, this, ()| {
            this.state.borrow_mut().lines.push(String::new());
            Ok(this.clone())
        });

        // Add multiple lines (from table or newline-separated string)
        methods.add_method("add_all", |_, this, lines: mlua::Value| {
            match lines {
                mlua::Value::Table(t) => {
                    for i in 1..=t.len()? {
                        let line: String = t.get(i)?;
                        this.add_line(line);
                    }
                }
                mlua::Value::String(s) => {
                    for line in s.to_str()?.lines() {
                        this.add_line(line.to_string());
                    }
                }
                _ => {}
            }
            Ok(this.clone())
        });

        methods.add_method("indent", |_, this, ()| {
            this.state.borrow_mut().indent_level += 1;
            Ok(this.clone())
        });

        methods.add_method("dedent", |_, this, ()| {
            let mut state = this.state.borrow_mut();
            if state.indent_level > 0 {
                state.indent_level -= 1;
            }
            Ok(this.clone())
        });

        methods.add_method("set_indent", |_, this, indent: String| {
            this.state.borrow_mut().indent_str = indent;
            Ok(this.clone())
        });

        // Add a block with automatic indent/dedent
        // Usage: lines:block("if foo {", "}", function(b) b:add("...") end)
        methods.add_method(
            "block",
            |_lua, this, (open, close, body): (String, String, mlua::Function)| {
                this.add_line(open);

                this.state.borrow_mut().indent_level += 1;
                body.call::<()>(this.clone())?;
                this.state.borrow_mut().indent_level -= 1;

                this.add_line(close);

                Ok(this.clone())
            },
        );

        methods.add_method("build", |_, this, ()| {
            Ok(this.state.borrow().lines.join("\n"))
        });

        methods.add_meta_method(mlua::MetaMethod::ToString, |_, this, ()| {
            Ok(this.state.borrow().lines.join("\n"))
        });

        methods.add_method("len", |_, this, ()| Ok(this.state.borrow().lines.len()));
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snake_case() {
        assert_eq!(convert_to_snake_case("fooBar"), "foo_bar");
        assert_eq!(convert_to_snake_case("FooBar"), "foo_bar");
        assert_eq!(convert_to_snake_case("foo_bar"), "foo_bar");
        assert_eq!(convert_to_snake_case("foo-bar"), "foo_bar");
        assert_eq!(convert_to_snake_case("FOO"), "foo");
        assert_eq!(convert_to_snake_case("createTodoRequest"), "create_todo_request");
    }

    #[test]
    fn test_pascal_case() {
        assert_eq!(convert_to_pascal_case("foo_bar"), "FooBar");
        assert_eq!(convert_to_pascal_case("foo-bar"), "FooBar");
        assert_eq!(convert_to_pascal_case("fooBar"), "FooBar");
        assert_eq!(convert_to_pascal_case("FooBar"), "FooBar");
    }

    #[test]
    fn test_helpers_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua
            .load("helpers.to_snake_case('FooBar')")
            .eval()
            .unwrap();
        assert_eq!(result, "foo_bar");

        let result: String = lua
            .load("helpers.to_pascal_case('foo_bar')")
            .eval()
            .unwrap();
        assert_eq!(result, "FooBar");

        let result: String = lua
            .load("helpers.to_camel_case('foo_bar')")
            .eval()
            .unwrap();
        assert_eq!(result, "fooBar");

        let result: String = lua
            .load("helpers.to_kebab_case('FooBar')")
            .eval()
            .unwrap();
        assert_eq!(result, "foo-bar");
    }

    #[test]
    fn test_template_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua
            .load(r#"helpers.template("Hello ${name}!", {name = "World"})"#)
            .eval()
            .unwrap();
        assert_eq!(result, "Hello World!");

        let result: String = lua
            .load(r#"helpers.template("${a} + ${b} = ${c}", {a = 1, b = 2, c = 3})"#)
            .eval()
            .unwrap();
        assert_eq!(result, "1 + 2 = 3");
    }

    #[test]
    fn test_line_builder_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua
            .load(
                r#"
                helpers.lines()
                    :add("fn main() {")
                    :indent()
                    :add("println!(\"Hello\");")
                    :dedent()
                    :add("}")
                    :build()
            "#,
            )
            .eval()
            .unwrap();

        assert_eq!(result, "fn main() {\n    println!(\"Hello\");\n}");
    }

    #[test]
    fn test_line_builder_block_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua
            .load(
                r#"
                helpers.lines()
                    :block("struct Foo {", "}", function(b)
                        b:add("field: i32,")
                    end)
                    :build()
            "#,
            )
            .eval()
            .unwrap();

        assert_eq!(result, "struct Foo {\n    field: i32,\n}");
    }

    #[test]
    fn test_indent_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua
            .load("helpers.indent('foo\\nbar', 2)")
            .eval()
            .unwrap();
        assert_eq!(result, "  foo\n  bar");
    }

    #[test]
    fn test_trim_in_lua() {
        let lua = Lua::new();
        register_helpers(&lua).unwrap();

        let result: String = lua.load("helpers.trim('  foo  ')").eval().unwrap();
        assert_eq!(result, "foo");
    }
}
