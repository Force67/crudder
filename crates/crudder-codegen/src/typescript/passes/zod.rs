//! Zod validation schema pass for TypeScript.

use crudder_ast::{Dto, PrimitiveType, Schema, TypeRef};

use crate::pass::{GenerationContext, Pass};
use crate::CodegenError;

/// Zod pass that generates Zod validation schemas.
///
/// This pass creates a `schemas.ts` file with Zod schemas for all DTOs.
/// It depends on the `typescript-base` pass to have run first.
pub struct ZodPass;

impl Pass for ZodPass {
    fn name(&self) -> &'static str {
        "zod"
    }

    fn depends_on(&self) -> &[&'static str] {
        &["typescript-base"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        let mut output = String::from(
            r#"// Generated Zod schemas from Crudder schema

import { z } from "zod";

"#,
        );

        for dto in &schema.dtos {
            output.push_str(&generate_zod_schema(dto));
            output.push('\n');
        }

        ctx.set_file("schemas.ts", output);

        // Signal to other passes that Zod is available
        ctx.set_metadata("has:zod", "true");

        Ok(())
    }
}

/// Generates a Zod schema for a DTO.
fn generate_zod_schema(dto: &Dto) -> String {
    let schema_name = format!("{}Schema", dto.name);
    let mut code = format!("export const {} = z.object({{\n", schema_name);

    for field in &dto.fields {
        let zod_type = type_ref_to_zod(&field.ty);
        let field_name = to_camel_case(&field.name);
        code.push_str(&format!("  {}: {},\n", field_name, zod_type));
    }

    code.push_str("});\n");
    code.push_str(&format!(
        "export type {} = z.infer<typeof {}>;\n",
        dto.name, schema_name
    ));
    code
}

/// Converts a TypeRef to a Zod validator.
fn type_ref_to_zod(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => primitive_to_zod(p),
        TypeRef::Array(inner) => {
            let inner_zod = type_ref_to_zod(inner);
            format!("{}.array()", inner_zod)
        }
        TypeRef::Optional(inner) => {
            let inner_zod = type_ref_to_zod(inner);
            format!("{}.optional()", inner_zod)
        }
        TypeRef::Named(name) => format!("{}Schema", name),
    }
}

/// Converts a primitive type to Zod.
fn primitive_to_zod(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::String => "z.string()".to_string(),
        PrimitiveType::Int => "z.number().int()".to_string(),
        PrimitiveType::Float => "z.number()".to_string(),
        PrimitiveType::Bool => "z.boolean()".to_string(),
        PrimitiveType::Uuid => "z.string().uuid()".to_string(),
        PrimitiveType::Cuid2 => "z.string().cuid2()".to_string(),
        PrimitiveType::Timestamp => "z.coerce.date()".to_string(),
        PrimitiveType::Bytes => "z.instanceof(Uint8Array)".to_string(),
    }
}

/// Converts a string to camelCase.
fn to_camel_case(s: &str) -> String {
    crate::typescript::base::to_camel_case(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::Field;

    #[test]
    fn test_generate_zod_schema() {
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

        let code = generate_zod_schema(&dto);
        assert!(code.contains("export const UserSchema = z.object"));
        assert!(code.contains("id: z.string().uuid()"));
        assert!(code.contains("name: z.string()"));
        assert!(code.contains("export type User = z.infer<typeof UserSchema>"));
    }
}
