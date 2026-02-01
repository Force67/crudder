//! Rust base pass - generates pure Rust structs.

use crudder_ast::{Dto, Field, PrimitiveType, Schema, TypeRef};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::pass::{GenerationContext, Pass};
use crate::CodegenError;

/// Rust base pass that generates pure Rust structs.
///
/// This pass creates a `src/types.rs` file with Rust structs for all DTOs
/// in the schema. The generated structs have only `Debug` and `Clone` derives.
/// Additional derives (Serialize, FromRow, etc.) are added by feature passes.
pub struct RustBasePass;

impl Pass for RustBasePass {
    fn name(&self) -> &'static str {
        "rust-base"
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        // Register structs and their base derives
        for dto in &schema.dtos {
            ctx.add_derive(&dto.name, "Debug");
            ctx.add_derive(&dto.name, "Clone");
            ctx.set_metadata(format!("dto:{}", dto.name), "true");
            if dto.is_entity() {
                ctx.set_metadata(format!("entity:{}", dto.name), "true");
                if let Some(table) = dto.table_name() {
                    ctx.set_metadata(format!("table:{}", dto.name), table.to_string());
                }
            }
        }

        // Mark services for other passes
        for service in &schema.services {
            ctx.set_metadata(format!("service:{}", service.name), "true");
            for method in &service.methods {
                ctx.set_metadata(format!("method:{}:{}", service.name, method.name), "true");
            }
        }

        // Generate types - but we do this deferred since derives may be added later
        // Store the schema info in metadata for the finalize step
        let types = generate_pure_types(&schema.dtos, ctx)?;
        ctx.set_file("src/types.rs", types);

        Ok(())
    }
}

/// Generates pure Rust structs for all DTOs.
fn generate_pure_types(dtos: &[Dto], ctx: &GenerationContext) -> Result<String, CodegenError> {
    let dto_structs: Vec<TokenStream> = dtos
        .iter()
        .map(|dto| generate_dto_struct(dto, ctx))
        .collect();

    let tokens = quote! {
        //! Generated types from Crudder schema.

        #(#dto_structs)*
    };

    super::format_rust(tokens)
}

/// Generates a struct for a DTO.
fn generate_dto_struct(dto: &Dto, ctx: &GenerationContext) -> TokenStream {
    let name = format_ident!("{}", dto.name);
    let fields: Vec<TokenStream> = dto.fields.iter().map(generate_field).collect();

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

/// Generates a struct field.
///
/// The base pass generates fields without serde attributes.
/// The serde pass will regenerate types with proper serde renames.
fn generate_field(field: &Field) -> TokenStream {
    let name = format_ident!("{}", to_snake_case(&field.name));
    let ty = type_ref_to_rust(&field.ty);

    quote! {
        pub #name: #ty,
    }
}

/// Converts a TypeRef to a Rust type.
pub fn type_ref_to_rust(ty: &TypeRef) -> TokenStream {
    match ty {
        TypeRef::Primitive(p) => primitive_to_rust(p),
        TypeRef::Array(inner) => {
            let inner_ty = type_ref_to_rust(inner);
            quote! { Vec<#inner_ty> }
        }
        TypeRef::Optional(inner) => {
            let inner_ty = type_ref_to_rust(inner);
            quote! { Option<#inner_ty> }
        }
        TypeRef::Named(name) => {
            let ident = format_ident!("{}", name);
            quote! { #ident }
        }
    }
}

/// Converts a primitive type to Rust.
pub fn primitive_to_rust(p: &PrimitiveType) -> TokenStream {
    match p {
        PrimitiveType::String => quote! { String },
        PrimitiveType::Int => quote! { i64 },
        PrimitiveType::Float => quote! { f64 },
        PrimitiveType::Bool => quote! { bool },
        PrimitiveType::Uuid => quote! { Uuid },
        PrimitiveType::Cuid2 => quote! { String },
        PrimitiveType::Timestamp => quote! { DateTime<Utc> },
        PrimitiveType::Bytes => quote! { Vec<u8> },
    }
}

/// Converts a PascalCase or camelCase string to snake_case.
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

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("CreateUser"), "create_user");
        assert_eq!(to_snake_case("UserService"), "user_service");
        assert_eq!(to_snake_case("ID"), "i_d");
        assert_eq!(to_snake_case("getUserByID"), "get_user_by_i_d");
    }

    #[test]
    fn test_primitive_to_rust() {
        let ts = primitive_to_rust(&PrimitiveType::String);
        assert_eq!(ts.to_string(), "String");

        let ts = primitive_to_rust(&PrimitiveType::Int);
        assert_eq!(ts.to_string(), "i64");
    }
}
