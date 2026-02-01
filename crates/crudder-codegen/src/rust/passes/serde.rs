//! Serde serialization pass for Rust.

use crudder_ast::Schema;

use crate::pass::{GenerationContext, Pass};
use crate::CodegenError;

/// Serde pass that adds Serialize/Deserialize derives to all structs.
///
/// This pass adds serde derives to all DTOs and registers the serde
/// dependency for Cargo.toml generation.
pub struct SerdePass;

impl Pass for SerdePass {
    fn name(&self) -> &'static str {
        "serde"
    }

    fn depends_on(&self) -> &[&'static str] {
        &["rust-base"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        // Add Serialize, Deserialize derives to all structs
        for dto in &schema.dtos {
            ctx.add_derive(&dto.name, "serde::Serialize");
            ctx.add_derive(&dto.name, "serde::Deserialize");
        }

        // Note: We don't use ctx.add_import here because regenerate_types
        // embeds the imports directly in the generated code.

        // Add serde to Cargo.toml dependencies
        ctx.set_metadata(
            "cargo:dep:serde",
            "{ version = \"1\", features = [\"derive\"] }",
        );
        ctx.set_metadata("cargo:dep:serde_json", "\"1\"");

        // Signal that serde is available
        ctx.set_metadata("has:serde", "true");

        // Regenerate types with new derives
        regenerate_types(schema, ctx)?;

        Ok(())
    }
}

/// Regenerate the types.rs file with updated derives.
fn regenerate_types(schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
    use crate::rust::base::{to_snake_case, type_ref_to_rust};
    use crudder_ast::{Dto, Field};
    use proc_macro2::TokenStream;
    use quote::{format_ident, quote};

    fn generate_dto_struct(dto: &Dto, ctx: &GenerationContext) -> TokenStream {
        let name = format_ident!("{}", dto.name);
        let fields: Vec<TokenStream> = dto.fields.iter().map(|f| generate_field(f)).collect();

        // Get derives from context
        let derives: Vec<TokenStream> = ctx
            .get_derives(&dto.name)
            .unwrap_or(&[])
            .iter()
            .map(|d| {
                let derive_path: TokenStream = d.parse().unwrap_or_else(|_| {
                    let ident = format_ident!("{}", d);
                    quote! { #ident }
                });
                derive_path
            })
            .collect();

        quote! {
            #[derive(#(#derives),*)]
            pub struct #name {
                #(#fields)*
            }
        }
    }

    fn generate_field(field: &Field) -> TokenStream {
        let name = format_ident!("{}", to_snake_case(&field.name));
        let ty = type_ref_to_rust(&field.ty);
        let original_name = &field.name;
        let snake_name = to_snake_case(original_name);

        if snake_name != *original_name {
            quote! {
                #[serde(rename = #original_name)]
                pub #name: #ty,
            }
        } else {
            quote! {
                pub #name: #ty,
            }
        }
    }

    let dto_structs: Vec<TokenStream> = schema
        .dtos
        .iter()
        .map(|dto| generate_dto_struct(dto, ctx))
        .collect();

    let tokens = quote! {
        //! Generated types from Crudder schema.

        use serde::{Deserialize, Serialize};
        use uuid::Uuid;
        use chrono::{DateTime, Utc};

        #(#dto_structs)*
    };

    let code = crate::rust::format_rust(tokens)?;
    ctx.set_file("src/types.rs", code);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pass::GenerationContext;
    use crudder_ast::{Dto, Field, PrimitiveType, TypeRef};

    #[test]
    fn test_serde_pass_adds_derives() {
        let schema = Schema {
            dtos: vec![Dto {
                name: "User".to_string(),
                fields: vec![Field {
                    name: "id".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::Uuid),
                    annotations: vec![],
                    index: None,
                    span: None,
                }],
                annotations: vec![],
                span: None,
            }],
            services: vec![],
        };

        let mut ctx = GenerationContext::new();
        ctx.add_derive("User", "Debug");
        ctx.add_derive("User", "Clone");

        let pass = SerdePass;
        pass.run(&schema, &mut ctx).unwrap();

        let derives = ctx.get_derives("User").unwrap();
        assert!(derives.contains(&"serde::Serialize".to_string()));
        assert!(derives.contains(&"serde::Deserialize".to_string()));
        assert!(ctx.has_metadata("has:serde"));
    }
}
