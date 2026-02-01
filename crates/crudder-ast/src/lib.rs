//! Core AST types for the Crudder DSL.

/// Primitive types supported by Crudder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    String,
    Int,
    Float,
    Bool,
    Uuid,
    Cuid2,
    Timestamp,
    Bytes,
}

impl PrimitiveType {
    /// Returns the keyword string for this primitive type.
    pub fn as_str(&self) -> &'static str {
        match self {
            PrimitiveType::String => "string",
            PrimitiveType::Int => "int",
            PrimitiveType::Float => "float",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Uuid => "uuid",
            PrimitiveType::Cuid2 => "cuid2",
            PrimitiveType::Timestamp => "timestamp",
            PrimitiveType::Bytes => "bytes",
        }
    }
}

/// Type references in Crudder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    /// A primitive type like `string`, `int`, etc.
    Primitive(PrimitiveType),
    /// An array type like `[]string` or `[]User`.
    Array(Box<TypeRef>),
    /// An optional type like `string?` or `User?`.
    Optional(Box<TypeRef>),
    /// A reference to a named DTO.
    Named(String),
}

/// An annotation with a name and optional arguments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    /// The annotation name (e.g., `rest`, `table`, `primary`).
    pub name: String,
    /// The annotation arguments (e.g., `["POST", "/users"]` or `["todos"]`).
    pub args: Vec<String>,
    /// Source span for error reporting (start, end).
    pub span: Option<(usize, usize)>,
}

impl Annotation {
    /// Creates a new annotation with no arguments.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            args: vec![],
            span: None,
        }
    }

    /// Creates a new annotation with arguments.
    pub fn with_args(name: impl Into<String>, args: Vec<String>) -> Self {
        Self {
            name: name.into(),
            args,
            span: None,
        }
    }

    /// Returns true if this is a `@primary` annotation.
    pub fn is_primary(&self) -> bool {
        self.name == "primary"
    }

    /// Returns true if this is an `@auto` annotation.
    pub fn is_auto(&self) -> bool {
        self.name == "auto"
    }

    /// Returns the table name if this is a `@table` annotation.
    pub fn table_name(&self) -> Option<&str> {
        if self.name == "table" {
            self.args.first().map(|s| s.as_str())
        } else {
            None
        }
    }

    /// Returns the column name if this is a `@column` annotation.
    pub fn column_name(&self) -> Option<&str> {
        if self.name == "column" {
            self.args.first().map(|s| s.as_str())
        } else {
            None
        }
    }

    /// Returns the referenced DTO name if this is a `@references` annotation.
    pub fn references(&self) -> Option<&str> {
        if self.name == "references" {
            self.args.first().map(|s| s.as_str())
        } else {
            None
        }
    }

    /// Returns the storage type if this is a `@storage` annotation.
    pub fn storage_type(&self) -> Option<StorageType> {
        if self.name == "storage" {
            self.args.first().and_then(|s| match s.as_str() {
                "memory" => Some(StorageType::Memory),
                "postgres" => Some(StorageType::Postgres),
                "sqlite" => Some(StorageType::Sqlite),
                _ => None,
            })
        } else {
            None
        }
    }
}

/// Storage backend type for services.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageType {
    /// In-memory storage (Vec with RwLock).
    Memory,
    /// PostgreSQL with SQLx.
    Postgres,
    /// SQLite with SQLx.
    Sqlite,
}

/// A field in a DTO.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    /// The field name.
    pub name: String,
    /// The field type.
    pub ty: TypeRef,
    /// Annotations on this field (e.g., `@primary`, `@column`).
    pub annotations: Vec<Annotation>,
    /// Optional field index for binary serialization formats (e.g., protobuf).
    pub index: Option<u32>,
    /// Source span for error reporting (start, end).
    pub span: Option<(usize, usize)>,
}

impl Field {
    /// Returns true if this field has a `@primary` annotation.
    pub fn is_primary(&self) -> bool {
        self.annotations.iter().any(|a| a.is_primary())
    }

    /// Returns true if this field has an `@auto` annotation.
    pub fn is_auto(&self) -> bool {
        self.annotations.iter().any(|a| a.is_auto())
    }

    /// Returns the column name (from `@column` or the field name).
    pub fn column_name(&self) -> &str {
        self.annotations
            .iter()
            .find_map(|a| a.column_name())
            .unwrap_or(&self.name)
    }

    /// Returns the referenced DTO name if this field has a `@references` annotation.
    pub fn references(&self) -> Option<&str> {
        self.annotations.iter().find_map(|a| a.references())
    }
}

/// A Data Transfer Object definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dto {
    /// The DTO name (e.g., `CreateUserRequest`).
    pub name: String,
    /// The fields in this DTO.
    pub fields: Vec<Field>,
    /// Annotations on this DTO (e.g., `@table`).
    pub annotations: Vec<Annotation>,
    /// Source span for error reporting (start, end).
    pub span: Option<(usize, usize)>,
}

impl Dto {
    /// Returns the table name if this DTO has a `@table` annotation.
    pub fn table_name(&self) -> Option<&str> {
        self.annotations.iter().find_map(|a| a.table_name())
    }

    /// Returns true if this DTO is a database entity (has `@table`).
    pub fn is_entity(&self) -> bool {
        self.table_name().is_some()
    }

    /// Returns the primary key field, if any.
    pub fn primary_key(&self) -> Option<&Field> {
        self.fields.iter().find(|f| f.is_primary())
    }
}

/// Authentication/authorization requirements for a method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthRequirement {
    /// No authentication required.
    Public,
    /// Requires an authenticated user.
    Authenticated,
    /// Requires the user to own the resource (field must match user.id).
    Owner(String),
    /// Requires a specific role.
    Role(String),
}

/// A method in a service.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    /// The method name (e.g., `CreateUser`).
    pub name: String,
    /// The input DTO name.
    pub input: String,
    /// The output DTO name, or `None` for void return.
    pub output: Option<String>,
    /// Annotations on this method.
    pub annotations: Vec<Annotation>,
    /// Source span for error reporting (start, end).
    pub span: Option<(usize, usize)>,
}

impl Method {
    /// Returns the auth requirement for this method, if specified.
    /// Defaults to `None` (no auth annotation = auth required by default).
    pub fn auth_requirement(&self) -> Option<AuthRequirement> {
        for ann in &self.annotations {
            match ann.name.as_str() {
                "public" => return Some(AuthRequirement::Public),
                "authenticated" => return Some(AuthRequirement::Authenticated),
                "owner" => {
                    let field = ann.args.first().cloned().unwrap_or_else(|| "user_id".to_string());
                    return Some(AuthRequirement::Owner(field));
                }
                "role" => {
                    if let Some(role) = ann.args.first() {
                        return Some(AuthRequirement::Role(role.clone()));
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Returns true if this method is public (no auth required).
    pub fn is_public(&self) -> bool {
        matches!(self.auth_requirement(), Some(AuthRequirement::Public))
    }

    /// Returns true if this method requires authentication.
    pub fn requires_auth(&self) -> bool {
        !self.is_public()
    }
}

/// A service definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Service {
    /// The service name (e.g., `UserService`).
    pub name: String,
    /// The methods in this service.
    pub methods: Vec<Method>,
    /// Annotations on this service (e.g., `@storage`).
    pub annotations: Vec<Annotation>,
    /// Source span for error reporting (start, end).
    pub span: Option<(usize, usize)>,
}

impl Service {
    /// Returns the storage type for this service, if specified.
    pub fn storage_type(&self) -> Option<StorageType> {
        self.annotations.iter().find_map(|a| a.storage_type())
    }
}

/// The root AST node representing an entire schema.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Schema {
    /// All DTO definitions.
    pub dtos: Vec<Dto>,
    /// All service definitions.
    pub services: Vec<Service>,
}

impl Schema {
    /// Creates a new empty schema.
    pub fn new() -> Self {
        Self::default()
    }

    /// Looks up a DTO by name.
    pub fn get_dto(&self, name: &str) -> Option<&Dto> {
        self.dtos.iter().find(|d| d.name == name)
    }

    /// Looks up a service by name.
    pub fn get_service(&self, name: &str) -> Option<&Service> {
        self.services.iter().find(|s| s.name == name)
    }

    /// Returns all DTOs that are database entities (have `@table`).
    pub fn entities(&self) -> impl Iterator<Item = &Dto> {
        self.dtos.iter().filter(|d| d.is_entity())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_as_str() {
        assert_eq!(PrimitiveType::String.as_str(), "string");
        assert_eq!(PrimitiveType::Uuid.as_str(), "uuid");
    }

    #[test]
    fn test_schema_lookup() {
        let schema = Schema {
            dtos: vec![Dto {
                name: "User".to_string(),
                fields: vec![],
                annotations: vec![],
                span: None,
            }],
            services: vec![Service {
                name: "UserService".to_string(),
                methods: vec![],
                annotations: vec![],
                span: None,
            }],
        };

        assert!(schema.get_dto("User").is_some());
        assert!(schema.get_dto("NotFound").is_none());
        assert!(schema.get_service("UserService").is_some());
    }

    #[test]
    fn test_field_annotations() {
        let field = Field {
            name: "id".to_string(),
            ty: TypeRef::Primitive(PrimitiveType::Uuid),
            annotations: vec![
                Annotation::new("primary"),
                Annotation::new("auto"),
            ],
            index: None,
            span: None,
        };

        assert!(field.is_primary());
        assert!(field.is_auto());
        assert_eq!(field.column_name(), "id");
    }

    #[test]
    fn test_column_rename() {
        let field = Field {
            name: "createdAt".to_string(),
            ty: TypeRef::Primitive(PrimitiveType::Timestamp),
            annotations: vec![Annotation::with_args("column", vec!["created_at".to_string()])],
            index: None,
            span: None,
        };

        assert_eq!(field.column_name(), "created_at");
    }

    #[test]
    fn test_dto_table() {
        let dto = Dto {
            name: "Todo".to_string(),
            fields: vec![],
            annotations: vec![Annotation::with_args("table", vec!["todos".to_string()])],
            span: None,
        };

        assert!(dto.is_entity());
        assert_eq!(dto.table_name(), Some("todos"));
    }

    #[test]
    fn test_service_storage() {
        let service = Service {
            name: "TodoService".to_string(),
            methods: vec![],
            annotations: vec![Annotation::with_args("storage", vec!["postgres".to_string()])],
            span: None,
        };

        assert_eq!(service.storage_type(), Some(StorageType::Postgres));
    }

    #[test]
    fn test_method_auth() {
        let public_method = Method {
            name: "ListTodos".to_string(),
            input: "Empty".to_string(),
            output: Some("TodoList".to_string()),
            annotations: vec![Annotation::new("public")],
            span: None,
        };
        assert_eq!(public_method.auth_requirement(), Some(AuthRequirement::Public));
        assert!(public_method.is_public());

        let owner_method = Method {
            name: "UpdateTodo".to_string(),
            input: "UpdateTodoRequest".to_string(),
            output: Some("Todo".to_string()),
            annotations: vec![Annotation::with_args("owner", vec!["userId".to_string()])],
            span: None,
        };
        assert_eq!(owner_method.auth_requirement(), Some(AuthRequirement::Owner("userId".to_string())));
        assert!(!owner_method.is_public());

        let role_method = Method {
            name: "DeleteAll".to_string(),
            input: "Empty".to_string(),
            output: None,
            annotations: vec![Annotation::with_args("role", vec!["admin".to_string()])],
            span: None,
        };
        assert_eq!(role_method.auth_requirement(), Some(AuthRequirement::Role("admin".to_string())));
    }
}
