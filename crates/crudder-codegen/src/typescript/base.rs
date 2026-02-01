//! TypeScript base pass - generates pure TypeScript interfaces.

use crudder_ast::{Dto, PrimitiveType, Schema, TypeRef};

use crate::pass::{GenerationContext, Pass};
use crate::CodegenError;

/// TypeScript base pass that generates pure TypeScript interfaces.
///
/// This pass creates a `types.ts` file with TypeScript interfaces for all DTOs
/// in the schema. The generated code is library-agnostic and contains no
/// external dependencies.
pub struct TypeScriptBasePass;

impl Pass for TypeScriptBasePass {
    fn name(&self) -> &'static str {
        "typescript-base"
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        // Generate pure TypeScript interfaces
        let types_content = generate_pure_types(&schema.dtos);
        ctx.set_file("types.ts", types_content);

        // Mark which DTOs exist for other passes
        for dto in &schema.dtos {
            ctx.set_metadata(format!("dto:{}", dto.name), "true");
            if dto.is_entity() {
                ctx.set_metadata(format!("entity:{}", dto.name), "true");
            }
        }

        // Mark services for other passes
        for service in &schema.services {
            ctx.set_metadata(format!("service:{}", service.name), "true");
            for method in &service.methods {
                ctx.set_metadata(format!("method:{}:{}", service.name, method.name), "true");
            }
        }

        Ok(())
    }
}

/// Generates pure TypeScript interfaces for all DTOs.
fn generate_pure_types(dtos: &[Dto]) -> String {
    let mut output = String::from("// Generated types from Crudder schema\n\n");

    for dto in dtos {
        output.push_str(&generate_interface(dto));
        output.push('\n');
    }

    output
}

/// Generates a TypeScript interface for a single DTO.
fn generate_interface(dto: &Dto) -> String {
    let mut code = format!("export interface {} {{\n", dto.name);

    for field in &dto.fields {
        let ts_type = type_ref_to_typescript(&field.ty);
        let optional = matches!(&field.ty, TypeRef::Optional(_));
        let field_name = to_camel_case(&field.name);

        if optional {
            code.push_str(&format!("  {}?: {};\n", field_name, ts_type));
        } else {
            code.push_str(&format!("  {}: {};\n", field_name, ts_type));
        }
    }

    code.push_str("}\n");
    code
}

/// Converts a TypeRef to a TypeScript type.
pub fn type_ref_to_typescript(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => primitive_to_typescript(p),
        TypeRef::Array(inner) => {
            let inner_ts = type_ref_to_typescript(inner);
            format!("{}[]", inner_ts)
        }
        TypeRef::Optional(inner) => {
            // The optional marker is handled at the field level
            type_ref_to_typescript(inner)
        }
        TypeRef::Named(name) => name.clone(),
    }
}

/// Converts a primitive type to TypeScript.
pub fn primitive_to_typescript(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::String => "string".to_string(),
        PrimitiveType::Int => "number".to_string(),
        PrimitiveType::Float => "number".to_string(),
        PrimitiveType::Bool => "boolean".to_string(),
        PrimitiveType::Uuid => "string".to_string(),
        PrimitiveType::Cuid2 => "string".to_string(),
        PrimitiveType::Timestamp => "Date".to_string(),
        PrimitiveType::Bytes => "Uint8Array".to_string(),
    }
}

/// Converts a string to camelCase.
pub fn to_camel_case(s: &str) -> String {
    let snake = to_snake_case(s);
    let mut result = String::new();
    let mut capitalize_next = false;

    for c in snake.chars() {
        if c == '_' {
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

/// Converts a string to snake_case.
pub fn to_snake_case(s: &str) -> String {
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
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("CreateUser"), "createUser");
        assert_eq!(to_camel_case("get_user"), "getUser");
    }

    #[test]
    fn test_primitive_to_typescript() {
        assert_eq!(primitive_to_typescript(&PrimitiveType::String), "string");
        assert_eq!(primitive_to_typescript(&PrimitiveType::Int), "number");
        assert_eq!(primitive_to_typescript(&PrimitiveType::Bool), "boolean");
    }

    #[test]
    fn test_generate_interface() {
        let dto = Dto {
            name: "User".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::Uuid),
                    annotations: vec![],
                    index: None,
                    span: None,
                },
                Field {
                    name: "name".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::String),
                    annotations: vec![],
                    index: None,
                    span: None,
                },
            ],
            annotations: vec![],
            span: None,
        };

        let code = generate_interface(&dto);
        assert!(code.contains("export interface User"));
        assert!(code.contains("id: string"));
        assert!(code.contains("name: string"));
    }
}
