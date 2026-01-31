//! Protobuf code generator.

use crudder_ast::{Dto, Method, PrimitiveType, Schema, Service, TypeRef};

use crate::{CodeGenerator, CodegenError, GeneratedFile, GeneratedFiles};

/// Protobuf code generator.
pub struct ProtobufGenerator {
    /// Package name for the generated proto file.
    pub package: Option<String>,
    /// Proto syntax version.
    pub syntax: String,
}

impl ProtobufGenerator {
    /// Creates a new Protobuf generator with default settings.
    pub fn new() -> Self {
        Self {
            package: None,
            syntax: "proto3".to_string(),
        }
    }

    /// Sets the package name.
    pub fn with_package(mut self, package: impl Into<String>) -> Self {
        self.package = Some(package.into());
        self
    }
}

impl Default for ProtobufGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for ProtobufGenerator {
    fn generate(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError> {
        let mut files = GeneratedFiles::new();

        let proto_code = generate_proto(schema, &self.syntax, self.package.as_deref());
        files.add(GeneratedFile::new("service.proto", proto_code));

        Ok(files)
    }

    fn name(&self) -> &'static str {
        "protobuf"
    }
}

/// Generates a complete proto file from the schema.
fn generate_proto(schema: &Schema, syntax: &str, package: Option<&str>) -> String {
    let mut code = format!("syntax = \"{}\";\n\n", syntax);

    if let Some(pkg) = package {
        code.push_str(&format!("package {};\n\n", pkg));
    }

    // Generate messages for each DTO
    for dto in &schema.dtos {
        code.push_str(&generate_message(dto));
        code.push('\n');
    }

    // Generate services
    for service in &schema.services {
        code.push_str(&generate_service(service));
        code.push('\n');
    }

    code
}

/// Generates a protobuf message for a DTO.
fn generate_message(dto: &Dto) -> String {
    let mut code = format!("message {} {{\n", dto.name);

    for (i, field) in dto.fields.iter().enumerate() {
        let proto_type = type_ref_to_proto(&field.ty);
        let field_name = to_snake_case(&field.name);
        let field_number = i + 1;

        // Handle optional fields
        if matches!(&field.ty, TypeRef::Optional(_)) {
            code.push_str(&format!(
                "  optional {} {} = {};\n",
                proto_type, field_name, field_number
            ));
        } else {
            code.push_str(&format!(
                "  {} {} = {};\n",
                proto_type, field_name, field_number
            ));
        }
    }

    code.push_str("}\n");
    code
}

/// Converts a TypeRef to a protobuf type.
fn type_ref_to_proto(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => primitive_to_proto(p),
        TypeRef::Array(inner) => {
            let inner_proto = type_ref_to_proto(inner);
            format!("repeated {}", inner_proto)
        }
        TypeRef::Optional(inner) => {
            // Optional is handled at the field level in proto3
            type_ref_to_proto(inner)
        }
        TypeRef::Named(name) => name.clone(),
    }
}

/// Converts a primitive type to protobuf.
fn primitive_to_proto(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::String => "string".to_string(),
        PrimitiveType::Int => "int64".to_string(),
        PrimitiveType::Float => "double".to_string(),
        PrimitiveType::Bool => "bool".to_string(),
        PrimitiveType::Uuid => "string".to_string(), // UUIDs as strings
        PrimitiveType::Timestamp => "google.protobuf.Timestamp".to_string(),
        PrimitiveType::Bytes => "bytes".to_string(),
    }
}

/// Generates a protobuf service definition.
fn generate_service(service: &Service) -> String {
    let mut code = format!("service {} {{\n", service.name);

    for method in &service.methods {
        code.push_str(&generate_rpc(method));
    }

    code.push_str("}\n");
    code
}

/// Generates an RPC definition for a method.
fn generate_rpc(method: &Method) -> String {
    let output = method
        .output
        .as_ref()
        .map(|s| s.as_str())
        .unwrap_or("google.protobuf.Empty");

    format!(
        "  rpc {}({}) returns ({});\n",
        method.name, method.input, output
    )
}

/// Converts a string to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::Field;

    #[test]
    fn test_primitive_to_proto() {
        assert_eq!(primitive_to_proto(&PrimitiveType::String), "string");
        assert_eq!(primitive_to_proto(&PrimitiveType::Int), "int64");
        assert_eq!(primitive_to_proto(&PrimitiveType::Float), "double");
        assert_eq!(primitive_to_proto(&PrimitiveType::Bool), "bool");
        assert_eq!(primitive_to_proto(&PrimitiveType::Bytes), "bytes");
    }

    #[test]
    fn test_generate_message() {
        let dto = Dto {
            name: "User".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::Uuid),
                    annotations: vec![],
                    span: None,
                },
                Field {
                    name: "name".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::String),
                    annotations: vec![],
                    span: None,
                },
            ],
            annotations: vec![],
            span: None,
        };

        let code = generate_message(&dto);
        assert!(code.contains("message User"));
        assert!(code.contains("string id = 1"));
        assert!(code.contains("string name = 2"));
    }

    #[test]
    fn test_generate_service() {
        let service = Service {
            name: "UserService".to_string(),
            methods: vec![Method {
                name: "CreateUser".to_string(),
                input: "CreateUserRequest".to_string(),
                output: Some("UserResponse".to_string()),
                annotations: vec![],
                span: None,
            }],
            annotations: vec![],
            span: None,
        };

        let code = generate_service(&service);
        assert!(code.contains("service UserService"));
        assert!(code.contains("rpc CreateUser(CreateUserRequest) returns (UserResponse)"));
    }
}
