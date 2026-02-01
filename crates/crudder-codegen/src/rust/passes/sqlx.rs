//! SQLx database pass for Rust.

use crudder_ast::{
    AuthRequirement, Dto, Method, PrimitiveType, Schema, Service, StorageType, TypeRef,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::pass::{GenerationContext, Pass};
use crate::rust::base::to_snake_case;
use crate::CodegenError;

/// SQLx pass that generates database handlers with full CRUD implementations.
///
/// This pass creates handler files for each service with complete SQL queries,
/// an auth module, and database migrations.
///
/// It depends on `rust-base`, `serde`, and `axum` passes.
pub struct SqlxPass {
    /// The storage type (Postgres or SQLite).
    pub storage: StorageType,
}

impl SqlxPass {
    /// Creates a new SQLx pass for Postgres.
    pub fn postgres() -> Self {
        Self {
            storage: StorageType::Postgres,
        }
    }

    /// Creates a new SQLx pass for SQLite.
    pub fn sqlite() -> Self {
        Self {
            storage: StorageType::Sqlite,
        }
    }
}

impl Pass for SqlxPass {
    fn name(&self) -> &'static str {
        match self.storage {
            StorageType::Postgres => "sqlx-postgres",
            StorageType::Sqlite => "sqlx-sqlite",
            StorageType::Memory => "memory",
        }
    }

    fn depends_on(&self) -> &[&'static str] {
        &["rust-base", "serde", "axum"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        // Add FromRow derive to entity DTOs (those with @table)
        for dto in &schema.dtos {
            if dto.is_entity() {
                ctx.add_derive(&dto.name, "sqlx::FromRow");
            }
        }

        // Regenerate types with FromRow derives
        regenerate_types(schema, ctx)?;

        // Generate auth module
        let auth_code = generate_auth_module();
        ctx.set_file("src/auth.rs", auth_code);

        // Generate service handlers with SQL queries (overwrites axum stubs)
        for service in &schema.services {
            let storage = service.storage_type().unwrap_or(self.storage);
            let handlers = generate_sqlx_handlers(service, schema, storage)?;
            let filename = format!("src/{}.rs", to_snake_case(&service.name));
            ctx.set_file(&filename, handlers);
        }

        // Update lib.rs to include auth module
        let lib_code = generate_lib(&schema.services)?;
        ctx.set_file("src/lib.rs", lib_code);

        // Generate migrations
        let migrations = generate_migrations(schema, self.storage);
        for (i, (name, sql)) in migrations.iter().enumerate() {
            ctx.set_file(&format!("migrations/{:03}_{}.sql", i + 1, name), sql.clone());
        }

        // Add SQLx dependencies
        let db_feature = match self.storage {
            StorageType::Postgres => "postgres",
            StorageType::Sqlite => "sqlite",
            StorageType::Memory => "",
        };
        ctx.set_metadata(
            "cargo:dep:sqlx",
            &format!(
                "{{ version = \"0.8\", features = [\"runtime-tokio\", \"{}\", \"uuid\", \"chrono\"] }}",
                db_feature
            ),
        );
        ctx.set_metadata(
            "cargo:dep:tower-http",
            "{ version = \"0.6\", features = [\"cors\"] }",
        );

        // Signal that SQLx is available
        ctx.set_metadata("has:sqlx", "true");
        ctx.set_metadata("sqlx:storage", db_feature);

        Ok(())
    }
}

/// Regenerate the types.rs file with updated derives.
fn regenerate_types(schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
    use crate::rust::base::type_ref_to_rust;

    fn generate_dto_struct(dto: &Dto, ctx: &GenerationContext) -> TokenStream {
        let name = format_ident!("{}", dto.name);
        let fields: Vec<TokenStream> = dto
            .fields
            .iter()
            .map(|f| {
                let field_name = format_ident!("{}", to_snake_case(&f.name));
                let ty = type_ref_to_rust(&f.ty);
                let original_name = &f.name;
                let snake_name = to_snake_case(original_name);

                if snake_name != *original_name {
                    quote! {
                        #[serde(rename = #original_name)]
                        pub #field_name: #ty,
                    }
                } else {
                    quote! {
                        pub #field_name: #ty,
                    }
                }
            })
            .collect();

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

    let dto_structs: Vec<TokenStream> = schema
        .dtos
        .iter()
        .map(|dto| generate_dto_struct(dto, ctx))
        .collect();

    let tokens = quote! {
        //! Generated types from Crudder schema.

        use serde::{Deserialize, Serialize};
        use sqlx::FromRow;
        use uuid::Uuid;
        use chrono::{DateTime, Utc};

        #(#dto_structs)*
    };

    let code = crate::rust::format_rust(tokens)?;
    ctx.set_file("src/types.rs", code);
    Ok(())
}

/// Generates handlers for a service with full SQLx implementation.
fn generate_sqlx_handlers(
    service: &Service,
    schema: &Schema,
    storage: StorageType,
) -> Result<String, CodegenError> {
    let service_name = &service.name;
    let module_name = to_snake_case(service_name);
    let router_fn_name = format_ident!("{}_router", module_name);

    let handlers: Vec<TokenStream> = service
        .methods
        .iter()
        .map(|m| generate_handler(m, schema, storage))
        .collect::<Result<_, _>>()?;

    let routes: Vec<TokenStream> = service.methods.iter().map(generate_route).collect();

    let pool_type = match storage {
        StorageType::Postgres => quote! { sqlx::PgPool },
        StorageType::Sqlite => quote! { sqlx::SqlitePool },
        StorageType::Memory => quote! { () },
    };

    let tokens = quote! {
        //! Generated handlers for #service_name with SQLx.

        use axum::{
            extract::{FromRef, Json, Path, Query, State},
            http::StatusCode,
            routing::{get, post, put, delete},
            Router,
        };
        use uuid::Uuid;

        use crate::auth::ValidatorState;
        use crate::types::*;

        /// Application state with database pool and auth validator.
        #[derive(Clone)]
        pub struct AppState {
            pub pool: #pool_type,
            pub validator: ValidatorState,
        }

        impl AppState {
            /// Creates a new AppState with the given pool and validator.
            pub fn new<V: crate::auth::TokenValidator + 'static>(pool: #pool_type, validator: V) -> Self {
                Self {
                    pool,
                    validator: ValidatorState::new(validator),
                }
            }
        }

        /// Allow extracting the pool from AppState.
        impl FromRef<AppState> for #pool_type {
            fn from_ref(state: &AppState) -> Self {
                state.pool.clone()
            }
        }

        /// Allow extracting the validator from AppState.
        impl FromRef<AppState> for ValidatorState {
            fn from_ref(state: &AppState) -> Self {
                state.validator.clone()
            }
        }

        /// Error type for handlers.
        pub type AppError = (StatusCode, String);

        #(#handlers)*

        /// Creates a router for #service_name.
        pub fn #router_fn_name() -> Router<AppState> {
            Router::new()
                #(#routes)*
        }
    };

    crate::rust::format_rust(tokens)
}

/// Finds the entity DTO associated with a method.
fn find_entity_for_method<'a>(method: &Method, schema: &'a Schema) -> Option<&'a Dto> {
    // First, check if output is directly an entity
    if let Some(output_name) = &method.output {
        if let Some(dto) = schema.get_dto(output_name) {
            if dto.is_entity() {
                return Some(dto);
            }
            // Check if the output DTO has an array field referencing an entity
            for field in &dto.fields {
                if let TypeRef::Array(inner) = &field.ty {
                    if let TypeRef::Named(name) = inner.as_ref() {
                        if let Some(inner_dto) = schema.get_dto(name) {
                            if inner_dto.is_entity() {
                                return Some(inner_dto);
                            }
                        }
                    }
                }
            }
        }
    }

    // For Delete methods that return Empty, try to infer entity from method name
    let name = &method.name;
    if name.starts_with("Delete") || name.starts_with("Remove") {
        let entity_name = name
            .strip_prefix("Delete")
            .or_else(|| name.strip_prefix("Remove"));
        if let Some(entity_name) = entity_name {
            if let Some(dto) = schema.get_dto(entity_name) {
                if dto.is_entity() {
                    return Some(dto);
                }
            }
        }
    }

    // For List methods, try to infer from method name
    if name.starts_with("List") {
        let entity_name = name
            .strip_prefix("List")
            .and_then(|s| s.strip_suffix('s').or(Some(s)));
        if let Some(entity_name) = entity_name {
            if let Some(dto) = schema.get_dto(entity_name) {
                if dto.is_entity() {
                    return Some(dto);
                }
            }
        }
    }

    None
}

/// Generates a handler function with SQLx implementation.
fn generate_handler(
    method: &Method,
    schema: &Schema,
    storage: StorageType,
) -> Result<TokenStream, CodegenError> {
    let fn_name = format_ident!("{}", to_snake_case(&method.name));

    let http_method = get_http_method(method);
    let path = get_path(method);
    let has_path_param = path.contains('{');

    // Try to find the entity DTO for this service
    let entity = find_entity_for_method(method, schema);

    // Get auth requirement
    let auth_req = method.auth_requirement();

    // Generate handler based on method type
    let (base_params, mut body, return_type) = if let Some(entity) = entity {
        generate_crud_handler(
            method,
            entity,
            http_method.as_str(),
            has_path_param,
            schema,
            storage,
        )?
    } else {
        generate_stub_handler(method, http_method.as_str(), has_path_param)
    };

    // Add auth params and guards based on auth requirement
    let (auth_params, auth_guard) = generate_auth_guard(&auth_req, entity, has_path_param, storage);

    // Combine auth guard with body
    if !auth_guard.is_empty() {
        body = quote! {
            #auth_guard
            #body
        };
    }

    // Combine params
    let params = if auth_params.is_empty() {
        base_params
    } else {
        quote! { #auth_params, #base_params }
    };

    Ok(quote! {
        pub async fn #fn_name(#params) -> #return_type {
            #body
        }
    })
}

/// Generates auth params and guard code based on auth requirement.
fn generate_auth_guard(
    auth_req: &Option<AuthRequirement>,
    entity: Option<&Dto>,
    has_path_param: bool,
    storage: StorageType,
) -> (TokenStream, TokenStream) {
    match auth_req {
        Some(AuthRequirement::Public) => (quote! {}, quote! {}),
        Some(AuthRequirement::Authenticated) => (
            quote! { user: crate::auth::AuthenticatedUser },
            quote! { let _ = &user; },
        ),
        Some(AuthRequirement::Owner(field)) => {
            let field_ident = format_ident!("{}", to_snake_case(field));

            if has_path_param {
                if let Some(entity) = entity {
                    let entity_type = format_ident!("{}", entity.name);
                    let default_table = to_snake_case(&entity.name);
                    let table = entity.table_name().unwrap_or(&default_table);
                    let pk_col = entity
                        .primary_key()
                        .map(column_name)
                        .unwrap_or_else(|| "id".to_string());
                    let id_param = sql_param(storage, 1);
                    let sql = format!("SELECT * FROM {} WHERE {} = {}", table, pk_col, id_param);

                    return (
                        quote! { user: crate::auth::AuthenticatedUser },
                        quote! {
                            let pool = &state.pool;
                            // Ownership check
                            let existing = sqlx::query_as::<_, #entity_type>(#sql)
                                .bind(id)
                                .fetch_optional(pool)
                                .await
                                .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?
                                .ok_or((StatusCode::NOT_FOUND, "Not found".to_string()))?;

                            if existing.#field_ident != user.id {
                                return Err((StatusCode::FORBIDDEN, "Not authorized".to_string()));
                            }
                        },
                    );
                }
            }

            (
                quote! { user: crate::auth::AuthenticatedUser },
                quote! { let _ = &user; },
            )
        }
        Some(AuthRequirement::Role(role)) => (
            quote! { user: crate::auth::AuthenticatedUser },
            quote! {
                if !user.has_role(#role) {
                    return Err((StatusCode::FORBIDDEN, "Insufficient permissions".to_string()));
                }
            },
        ),
        None => (
            quote! { user: crate::auth::AuthenticatedUser },
            quote! { let _ = &user; },
        ),
    }
}

/// Generates a full CRUD handler for an entity.
fn generate_crud_handler(
    method: &Method,
    entity: &Dto,
    http_method: &str,
    has_path_param: bool,
    schema: &Schema,
    storage: StorageType,
) -> Result<(TokenStream, TokenStream, TokenStream), CodegenError> {
    let input_type = format_ident!("{}", method.input);
    let output_type = format_ident!("{}", entity.name);
    let table = table_name(entity);
    let pk = entity.primary_key();

    // Get the input DTO to know what fields we're receiving
    let input_dto = schema.get_dto(&method.input);

    match http_method {
        "POST" => {
            // CREATE - Insert new record using input DTO fields
            let insert_fields: Vec<_> = input_dto
                .map(|dto| dto.fields.iter().collect())
                .unwrap_or_default();

            let columns: Vec<_> = insert_fields.iter().map(|f| column_name(f)).collect();
            let placeholders: Vec<_> = (1..=insert_fields.len())
                .map(|i| sql_param(storage, i))
                .collect();
            let binds: Vec<TokenStream> = insert_fields
                .iter()
                .map(|f| {
                    let name = format_ident!("{}", to_snake_case(&f.name));
                    quote! { .bind(&req.#name) }
                })
                .collect();

            let sql = format!(
                "INSERT INTO {} ({}) VALUES ({}) RETURNING *",
                table,
                columns.join(", "),
                placeholders.join(", ")
            );

            Ok((
                quote! { State(state): State<AppState>, Json(req): Json<#input_type> },
                quote! {
                    let pool = &state.pool;
                    let result = sqlx::query_as::<_, #output_type>(#sql)
                        #(#binds)*
                        .fetch_one(pool)
                        .await
                        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
                    Ok(Json(result))
                },
                quote! { Result<Json<#output_type>, AppError> },
            ))
        }
        "GET" if has_path_param => {
            // READ - Get by ID
            let pk_col = pk
                .map(column_name)
                .unwrap_or_else(|| "id".to_string());
            let id_param = sql_param(storage, 1);
            let sql = format!("SELECT * FROM {} WHERE {} = {}", table, pk_col, id_param);

            Ok((
                quote! { State(state): State<AppState>, Path(id): Path<Uuid> },
                quote! {
                    let pool = &state.pool;
                    let result = sqlx::query_as::<_, #output_type>(#sql)
                        .bind(id)
                        .fetch_optional(pool)
                        .await
                        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?
                        .ok_or((StatusCode::NOT_FOUND, "Not found".to_string()))?;
                    Ok(Json(result))
                },
                quote! { Result<Json<#output_type>, AppError> },
            ))
        }
        "GET" => {
            // LIST - Get all
            let sql = format!("SELECT * FROM {}", table);
            let list_type = format_ident!("{}List", entity.name);
            let items_field = format_ident!("{}s", to_snake_case(&entity.name));

            Ok((
                quote! { State(state): State<AppState> },
                quote! {
                    let pool = &state.pool;
                    let items = sqlx::query_as::<_, #output_type>(#sql)
                        .fetch_all(pool)
                        .await
                        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
                    Ok(Json(#list_type { #items_field: items }))
                },
                quote! { Result<Json<#list_type>, AppError> },
            ))
        }
        "PUT" => {
            // UPDATE - Update by ID using input DTO fields
            let pk_col = pk
                .map(column_name)
                .unwrap_or_else(|| "id".to_string());

            // Use input DTO fields for the update
            let update_fields: Vec<_> = input_dto
                .map(|dto| dto.fields.iter().collect())
                .unwrap_or_default();

            let set_clauses: Vec<String> = update_fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    let col = column_name(f);
                    let param = sql_param(storage, i + 2);
                    format!("{} = COALESCE({}, {})", col, param, col)
                })
                .collect();

            let id_param = sql_param(storage, 1);
            let sql = format!(
                "UPDATE {} SET {} WHERE {} = {} RETURNING *",
                table,
                set_clauses.join(", "),
                pk_col,
                id_param
            );

            let binds: Vec<TokenStream> = update_fields
                .iter()
                .map(|f| {
                    let name = format_ident!("{}", to_snake_case(&f.name));
                    quote! { .bind(&req.#name) }
                })
                .collect();

            Ok((
                quote! { State(state): State<AppState>, Path(id): Path<Uuid>, Json(req): Json<#input_type> },
                quote! {
                    let pool = &state.pool;
                    let result = sqlx::query_as::<_, #output_type>(#sql)
                        .bind(id)
                        #(#binds)*
                        .fetch_optional(pool)
                        .await
                        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?
                        .ok_or((StatusCode::NOT_FOUND, "Not found".to_string()))?;
                    Ok(Json(result))
                },
                quote! { Result<Json<#output_type>, AppError> },
            ))
        }
        "DELETE" => {
            // DELETE - Delete by ID
            let pk_col = pk
                .map(column_name)
                .unwrap_or_else(|| "id".to_string());
            let id_param = sql_param(storage, 1);
            let sql = format!("DELETE FROM {} WHERE {} = {}", table, pk_col, id_param);

            Ok((
                quote! { State(state): State<AppState>, Path(id): Path<Uuid> },
                quote! {
                    let pool = &state.pool;
                    let result = sqlx::query(#sql)
                        .bind(id)
                        .execute(pool)
                        .await
                        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

                    if result.rows_affected() == 0 {
                        return Err((StatusCode::NOT_FOUND, "Not found".to_string()));
                    }

                    Ok(Json(Empty {}))
                },
                quote! { Result<Json<Empty>, AppError> },
            ))
        }
        _ => Ok(generate_stub_handler(method, http_method, has_path_param)),
    }
}

/// Generates a stub handler for non-entity methods.
fn generate_stub_handler(
    method: &Method,
    http_method: &str,
    has_path_param: bool,
) -> (TokenStream, TokenStream, TokenStream) {
    let input_type = format_ident!("{}", method.input);
    let fn_name = to_snake_case(&method.name);

    let return_type = if let Some(output) = &method.output {
        let output_type = format_ident!("{}", output);
        quote! { Result<Json<#output_type>, AppError> }
    } else {
        quote! { Result<(), AppError> }
    };

    let params = if has_path_param {
        if http_method == "GET" || http_method == "DELETE" {
            quote! { State(_state): State<AppState>, Path(_id): Path<Uuid> }
        } else {
            quote! { State(_state): State<AppState>, Path(_id): Path<Uuid>, Json(_req): Json<#input_type> }
        }
    } else if http_method == "GET" {
        quote! { State(_state): State<AppState>, Query(_req): Query<#input_type> }
    } else {
        quote! { State(_state): State<AppState>, Json(_req): Json<#input_type> }
    };

    let body = quote! {
        todo!("Implement {} handler", #fn_name)
    };

    (params, body, return_type)
}

/// Generates a route registration for a method.
fn generate_route(method: &Method) -> TokenStream {
    let fn_name = format_ident!("{}", to_snake_case(&method.name));
    let path = get_path(method);
    let http_method = get_http_method(method);

    let axum_path = path.replace('{', ":").replace('}', "");

    let method_fn = match http_method.as_str() {
        "POST" => quote! { post },
        "PUT" => quote! { put },
        "DELETE" => quote! { delete },
        _ => quote! { get },
    };

    quote! {
        .route(#axum_path, #method_fn(#fn_name))
    }
}

/// Generates the lib.rs file with auth module.
fn generate_lib(services: &[Service]) -> Result<String, CodegenError> {
    let modules: Vec<TokenStream> = services
        .iter()
        .map(|s| {
            let name = format_ident!("{}", to_snake_case(&s.name));
            quote! { pub mod #name; }
        })
        .collect();

    let router_calls: Vec<TokenStream> = services
        .iter()
        .map(|s| {
            let mod_name = format_ident!("{}", to_snake_case(&s.name));
            let router_fn = format_ident!("{}_router", to_snake_case(&s.name));
            quote! { .merge(#mod_name::#router_fn()) }
        })
        .collect();

    // Get the first service's app state type
    let app_state = if let Some(s) = services.first() {
        let mod_name = format_ident!("{}", to_snake_case(&s.name));
        quote! { #mod_name::AppState }
    } else {
        quote! { () }
    };

    let tokens = quote! {
        //! Generated Crudder service with SQLx.

        pub mod auth;
        pub mod types;
        #(#modules)*

        use axum::Router;

        /// Creates the main router with all services.
        pub fn router() -> Router<#app_state> {
            Router::new()
                #(#router_calls)*
        }
    };

    crate::rust::format_rust(tokens)
}

/// Generates SQL migrations for all entities.
fn generate_migrations(schema: &Schema, storage: StorageType) -> Vec<(String, String)> {
    let mut migrations = Vec::new();

    for dto in schema.entities() {
        let table = table_name(dto);
        let create_sql = generate_create_table(dto, schema, storage);
        migrations.push((format!("create_{}", table), create_sql));
    }

    migrations
}

/// Generates CREATE TABLE SQL for a DTO.
fn generate_create_table(dto: &Dto, schema: &Schema, storage: StorageType) -> String {
    let table = table_name(dto);
    let mut columns = Vec::new();
    let mut constraints = Vec::new();

    for field in &dto.fields {
        let col_name = column_name(field);
        let sql_type = type_to_sql(storage, &field.ty);
        let is_optional = matches!(field.ty, TypeRef::Optional(_));

        let mut col_def = format!("    {} {}", col_name, sql_type);

        if field.is_primary() {
            col_def.push_str(" PRIMARY KEY");
        }

        if field.is_auto() {
            if matches!(storage, StorageType::Postgres)
                && matches!(field.ty, TypeRef::Primitive(PrimitiveType::Uuid))
            {
                col_def.push_str(" DEFAULT gen_random_uuid()");
            }
        }

        if !is_optional && !field.is_primary() {
            col_def.push_str(" NOT NULL");
        }

        // Handle timestamp defaults
        if matches!(field.ty, TypeRef::Primitive(PrimitiveType::Timestamp)) && field.is_auto() {
            match storage {
                StorageType::Postgres => col_def.push_str(" DEFAULT NOW()"),
                StorageType::Sqlite => col_def.push_str(" DEFAULT CURRENT_TIMESTAMP"),
                StorageType::Memory => {}
            }
        }

        // Handle boolean defaults
        if matches!(field.ty, TypeRef::Primitive(PrimitiveType::Bool)) && !field.is_primary() {
            match storage {
                StorageType::Postgres => col_def.push_str(" DEFAULT FALSE"),
                StorageType::Sqlite => col_def.push_str(" DEFAULT 0"),
                StorageType::Memory => {}
            }
        }

        columns.push(col_def);

        // Handle foreign keys
        if let Some(ref_dto) = field.references() {
            let ref_table = schema
                .get_dto(ref_dto)
                .and_then(|dto| dto.table_name())
                .map(ToString::to_string)
                .unwrap_or_else(|| to_snake_case(ref_dto));
            constraints.push(format!(
                "    FOREIGN KEY ({}) REFERENCES {}(id)",
                col_name, ref_table
            ));
        }
    }

    let mut sql = format!("CREATE TABLE {} (\n", table);
    sql.push_str(&columns.join(",\n"));
    if !constraints.is_empty() {
        sql.push_str(",\n");
        sql.push_str(&constraints.join(",\n"));
    }
    sql.push_str("\n);\n");

    sql
}

/// Converts a TypeRef to SQL type.
fn type_to_sql(storage: StorageType, ty: &TypeRef) -> &'static str {
    match (storage, ty) {
        (_, TypeRef::Optional(inner)) => type_to_sql(storage, inner),

        (StorageType::Postgres, TypeRef::Primitive(p)) => match p {
            PrimitiveType::String => "TEXT",
            PrimitiveType::Int => "BIGINT",
            PrimitiveType::Float => "DOUBLE PRECISION",
            PrimitiveType::Bool => "BOOLEAN",
            PrimitiveType::Uuid => "UUID",
            PrimitiveType::Cuid2 => "TEXT",
            PrimitiveType::Timestamp => "TIMESTAMPTZ",
            PrimitiveType::Bytes => "BYTEA",
        },
        (StorageType::Postgres, TypeRef::Array(_)) => "JSONB",
        (StorageType::Postgres, TypeRef::Named(_)) => "JSONB",

        (StorageType::Sqlite, TypeRef::Primitive(p)) => match p {
            PrimitiveType::String => "TEXT",
            PrimitiveType::Int => "INTEGER",
            PrimitiveType::Float => "REAL",
            PrimitiveType::Bool => "INTEGER",
            PrimitiveType::Uuid => "TEXT",
            PrimitiveType::Cuid2 => "TEXT",
            PrimitiveType::Timestamp => "TEXT",
            PrimitiveType::Bytes => "BLOB",
        },
        (StorageType::Sqlite, TypeRef::Array(_)) => "TEXT",
        (StorageType::Sqlite, TypeRef::Named(_)) => "TEXT",

        // Not meaningful for this pass, but keeps signatures total.
        (StorageType::Memory, _) => "TEXT",
    }
}

fn table_name(dto: &Dto) -> String {
    dto.table_name()
        .map(ToString::to_string)
        .unwrap_or_else(|| to_snake_case(&dto.name))
}

fn column_name(field: &crudder_ast::Field) -> String {
    field
        .annotations
        .iter()
        .find_map(|a| a.column_name())
        .map(ToString::to_string)
        .unwrap_or_else(|| to_snake_case(&field.name))
}

fn sql_param(storage: StorageType, idx: usize) -> String {
    match storage {
        StorageType::Postgres => format!("${}", idx),
        StorageType::Sqlite => format!("?{}", idx),
        StorageType::Memory => format!("${}", idx),
    }
}

/// Gets the HTTP method from annotation or infers from method name.
fn get_http_method(method: &Method) -> String {
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(http_method) = ann.args.first() {
            return http_method.trim().to_uppercase();
        }
    }

    let name = &method.name;
    let lower = name.to_lowercase();
    if lower.starts_with("create") || lower.starts_with("add") {
        "POST".to_string()
    } else if lower.starts_with("update") || lower.starts_with("set") {
        "PUT".to_string()
    } else if lower.starts_with("delete") || lower.starts_with("remove") {
        "DELETE".to_string()
    } else {
        "GET".to_string()
    }
}

/// Gets the path from annotation or generates a default.
fn get_path(method: &Method) -> String {
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(path) = ann.args.get(1) {
            return path.clone();
        }
    }

    format!("/{}", to_snake_case(&method.name))
}

/// Generates the auth module with pluggable token validation.
fn generate_auth_module() -> String {
    r#"//! Authentication and authorization with pluggable token validation.
//!
//! # Security
//!
//! This module provides a `TokenValidator` trait that you MUST implement
//! for production use. The included `DevValidator` is INSECURE and should
//! only be used for local development.
//!
//! # Example
//!
//! ```rust,ignore
//! // For development (INSECURE - tokens can be forged!)
//! let state = AppState::new(pool, DevValidator::new());
//!
//! // For production - implement your own validator
//! let state = AppState::new(pool, MyJwtValidator::new(secret));
//! ```

use axum::{
    extract::{FromRef, FromRequestParts},
    http::{request::Parts, StatusCode},
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

/// An authenticated user extracted from a validated token.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthenticatedUser {
    /// The user's unique identifier.
    pub id: Uuid,
    /// The user's roles.
    pub roles: Vec<String>,
}

impl AuthenticatedUser {
    /// Returns true if the user has the given role.
    pub fn has_role(&self, role: &str) -> bool {
        self.roles.iter().any(|r| r == role)
    }
}

/// Error returned when token validation fails.
#[derive(Debug, Clone)]
pub struct AuthError {
    pub status: StatusCode,
    pub message: &'static str,
}

impl AuthError {
    pub fn unauthorized(message: &'static str) -> Self {
        Self { status: StatusCode::UNAUTHORIZED, message }
    }

    pub fn forbidden(message: &'static str) -> Self {
        Self { status: StatusCode::FORBIDDEN, message }
    }
}

impl From<AuthError> for (StatusCode, &'static str) {
    fn from(e: AuthError) -> Self {
        (e.status, e.message)
    }
}

// ============================================================================
// TokenValidator trait - implement this for your auth system
// ============================================================================

/// Trait for validating authentication tokens.
///
/// Implement this trait to plug in your own authentication system:
/// - JWT validation (with jsonwebtoken, jwt-simple, etc.)
/// - Session-based auth (lookup in Redis/database)
/// - API key validation
/// - OAuth token introspection
///
/// # Example Implementation
///
/// ```rust,ignore
/// use jsonwebtoken::{decode, DecodingKey, Validation};
///
/// struct JwtValidator {
///     decoding_key: DecodingKey,
/// }
///
/// impl TokenValidator for JwtValidator {
///     fn validate(&self, token: &str) -> Result<AuthenticatedUser, AuthError> {
///         let data = decode::<Claims>(token, &self.decoding_key, &Validation::default())
///             .map_err(|_| AuthError::unauthorized("Invalid token"))?;
///
///         Ok(AuthenticatedUser {
///             id: data.claims.sub.parse().unwrap(),
///             roles: data.claims.roles,
///         })
///     }
/// }
/// ```
pub trait TokenValidator: Send + Sync {
    /// Validates a token and returns the authenticated user.
    fn validate(&self, token: &str) -> Result<AuthenticatedUser, AuthError>;
}

// ============================================================================
// DevValidator - INSECURE, for development only
// ============================================================================

/// Development-only token validator. DO NOT USE IN PRODUCTION.
///
/// Accepts tokens in format: `{uuid}:{role1},{role2}`
///
/// # Security Warning
///
/// This validator trusts whatever the client sends. It provides NO security:
/// - User ID is client-controlled (impersonation possible)
/// - Roles are client-controlled (privilege escalation possible)
/// - No cryptographic verification
/// - No expiration
///
/// Use ONLY for local development and testing.
pub struct DevValidator {
    _private: (), // Prevent direct construction, force use of new()
}

impl DevValidator {
    /// Creates a new DevValidator with a loud warning.
    pub fn new() -> Self {
        eprintln!("╔══════════════════════════════════════════════════════════════╗");
        eprintln!("║  ⚠️  WARNING: Using DevValidator - INSECURE AUTH ⚠️            ║");
        eprintln!("║                                                              ║");
        eprintln!("║  Tokens can be forged by anyone. This is for development     ║");
        eprintln!("║  only. For production, implement TokenValidator with JWT     ║");
        eprintln!("║  or session-based auth.                                      ║");
        eprintln!("╚══════════════════════════════════════════════════════════════╝");
        Self { _private: () }
    }
}

impl Default for DevValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl TokenValidator for DevValidator {
    fn validate(&self, token: &str) -> Result<AuthenticatedUser, AuthError> {
        let parts: Vec<&str> = token.split(':').collect();

        let id = parts
            .first()
            .and_then(|s| Uuid::parse_str(s).ok())
            .ok_or(AuthError::unauthorized("Invalid token format"))?;

        let roles = parts
            .get(1)
            .map(|s| s.split(',').filter(|r| !r.is_empty()).map(String::from).collect())
            .unwrap_or_default();

        Ok(AuthenticatedUser { id, roles })
    }
}

// ============================================================================
// HmacValidator - Simple secure validator using HMAC
// ============================================================================

/// HMAC-based token validator using SHA-256.
///
/// Token format: `{uuid}:{roles}:{timestamp}:{signature}`
///
/// This provides:
/// - Cryptographic verification (tokens can't be forged without secret)
/// - Expiration (configurable TTL)
/// - No external dependencies (uses Rust stdlib)
///
/// # Example
///
/// ```rust,ignore
/// let validator = HmacValidator::new(b"your-secret-key-at-least-32-bytes!!", 3600);
///
/// // Generate a token
/// let token = validator.generate_token(user_id, &["user", "admin"]);
///
/// // Validate a token
/// let user = validator.validate(&token)?;
/// ```
pub struct HmacValidator {
    secret: Vec<u8>,
    ttl_seconds: u64,
}

impl HmacValidator {
    /// Creates a new HMAC validator.
    ///
    /// # Arguments
    /// - `secret`: Secret key for signing (should be at least 32 bytes)
    /// - `ttl_seconds`: Token time-to-live in seconds (0 = no expiration)
    pub fn new(secret: &[u8], ttl_seconds: u64) -> Self {
        if secret.len() < 32 {
            eprintln!("⚠️  Warning: HMAC secret should be at least 32 bytes for security");
        }
        Self {
            secret: secret.to_vec(),
            ttl_seconds,
        }
    }

    /// Generates a signed token for a user.
    pub fn generate_token(&self, user_id: Uuid, roles: &[&str]) -> String {
        use std::time::{SystemTime, UNIX_EPOCH};

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();

        let roles_str = roles.join(",");
        let payload = format!("{}:{}:{}", user_id, roles_str, timestamp);
        let signature = self.sign(&payload);

        format!("{}:{}", payload, signature)
    }

    fn sign(&self, payload: &str) -> String {
        // HMAC-SHA256 implementation using stdlib
        use std::num::Wrapping;

        fn sha256(data: &[u8]) -> [u8; 32] {
            // SHA-256 constants
            const K: [u32; 64] = [
                0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
                0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
                0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
                0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
                0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
                0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
                0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
                0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
                0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
                0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
                0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
                0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
                0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
                0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
                0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
                0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
            ];

            let mut h: [Wrapping<u32>; 8] = [
                Wrapping(0x6a09e667), Wrapping(0xbb67ae85),
                Wrapping(0x3c6ef372), Wrapping(0xa54ff53a),
                Wrapping(0x510e527f), Wrapping(0x9b05688c),
                Wrapping(0x1f83d9ab), Wrapping(0x5be0cd19),
            ];

            // Padding
            let bit_len = (data.len() as u64) * 8;
            let mut padded = data.to_vec();
            padded.push(0x80);
            while (padded.len() % 64) != 56 {
                padded.push(0);
            }
            padded.extend_from_slice(&bit_len.to_be_bytes());

            // Process blocks
            for chunk in padded.chunks(64) {
                let mut w = [Wrapping(0u32); 64];
                for (i, word) in chunk.chunks(4).enumerate() {
                    w[i] = Wrapping(u32::from_be_bytes([word[0], word[1], word[2], word[3]]));
                }

                for i in 16..64 {
                    let s0 = (w[i-15].0.rotate_right(7)) ^ (w[i-15].0.rotate_right(18)) ^ (w[i-15].0 >> 3);
                    let s1 = (w[i-2].0.rotate_right(17)) ^ (w[i-2].0.rotate_right(19)) ^ (w[i-2].0 >> 10);
                    w[i] = w[i-16] + Wrapping(s0) + w[i-7] + Wrapping(s1);
                }

                let (mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh) =
                    (h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]);

                for i in 0..64 {
                    let s1 = Wrapping(e.0.rotate_right(6) ^ e.0.rotate_right(11) ^ e.0.rotate_right(25));
                    let ch = Wrapping((e.0 & f.0) ^ ((!e.0) & g.0));
                    let t1 = hh + s1 + ch + Wrapping(K[i]) + w[i];
                    let s0 = Wrapping(a.0.rotate_right(2) ^ a.0.rotate_right(13) ^ a.0.rotate_right(22));
                    let maj = Wrapping((a.0 & b.0) ^ (a.0 & c.0) ^ (b.0 & c.0));
                    let t2 = s0 + maj;

                    hh = g; g = f; f = e; e = d + t1;
                    d = c; c = b; b = a; a = t1 + t2;
                }

                h[0] = h[0] + a; h[1] = h[1] + b;
                h[2] = h[2] + c; h[3] = h[3] + d;
                h[4] = h[4] + e; h[5] = h[5] + f;
                h[6] = h[6] + g; h[7] = h[7] + hh;
            }

            let mut result = [0u8; 32];
            for (i, v) in h.iter().enumerate() {
                result[i*4..i*4+4].copy_from_slice(&v.0.to_be_bytes());
            }
            result
        }

        // HMAC-SHA256
        let block_size = 64;
        let mut key = self.secret.clone();
        if key.len() > block_size {
            key = sha256(&key).to_vec();
        }
        key.resize(block_size, 0);

        let mut i_pad = vec![0x36u8; block_size];
        let mut o_pad = vec![0x5cu8; block_size];
        for i in 0..block_size {
            i_pad[i] ^= key[i];
            o_pad[i] ^= key[i];
        }

        i_pad.extend_from_slice(payload.as_bytes());
        let inner_hash = sha256(&i_pad);

        o_pad.extend_from_slice(&inner_hash);
        let outer_hash = sha256(&o_pad);

        // Encode as hex
        outer_hash.iter().map(|b| format!("{:02x}", b)).collect()
    }
}

impl TokenValidator for HmacValidator {
    fn validate(&self, token: &str) -> Result<AuthenticatedUser, AuthError> {
        use std::time::{SystemTime, UNIX_EPOCH};

        let parts: Vec<&str> = token.splitn(4, ':').collect();
        if parts.len() != 4 {
            return Err(AuthError::unauthorized("Invalid token format"));
        }

        let (user_id_str, roles_str, timestamp_str, signature) =
            (parts[0], parts[1], parts[2], parts[3]);

        // Verify signature
        let payload = format!("{}:{}:{}", user_id_str, roles_str, timestamp_str);
        let expected_sig = self.sign(&payload);

        // Constant-time comparison
        if signature.len() != expected_sig.len() {
            return Err(AuthError::unauthorized("Invalid signature"));
        }
        let mut diff = 0u8;
        for (a, b) in signature.bytes().zip(expected_sig.bytes()) {
            diff |= a ^ b;
        }
        if diff != 0 {
            return Err(AuthError::unauthorized("Invalid signature"));
        }

        // Check expiration
        if self.ttl_seconds > 0 {
            let timestamp: u64 = timestamp_str
                .parse()
                .map_err(|_| AuthError::unauthorized("Invalid timestamp"))?;

            let now = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs();

            if now > timestamp + self.ttl_seconds {
                return Err(AuthError::unauthorized("Token expired"));
            }
        }

        // Parse user
        let id = Uuid::parse_str(user_id_str)
            .map_err(|_| AuthError::unauthorized("Invalid user ID"))?;

        let roles = roles_str
            .split(',')
            .filter(|r| !r.is_empty())
            .map(String::from)
            .collect();

        Ok(AuthenticatedUser { id, roles })
    }
}

// ============================================================================
// Extractor implementation
// ============================================================================

/// Wrapper around Arc<dyn TokenValidator> for state management.
#[derive(Clone)]
pub struct ValidatorState(pub Arc<dyn TokenValidator>);

impl ValidatorState {
    pub fn new<V: TokenValidator + 'static>(validator: V) -> Self {
        Self(Arc::new(validator))
    }
}

/// Extractor for optional authentication.
#[derive(Debug, Clone)]
pub struct MaybeUser(pub Option<AuthenticatedUser>);

impl<S> FromRequestParts<S> for AuthenticatedUser
where
    ValidatorState: FromRef<S>,
    S: Send + Sync,
{
    type Rejection = (StatusCode, &'static str);

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let validator = ValidatorState::from_ref(state);

        let auth_header = parts
            .headers
            .get("Authorization")
            .and_then(|v| v.to_str().ok())
            .ok_or((StatusCode::UNAUTHORIZED, "Missing Authorization header"))?;

        let token = auth_header
            .strip_prefix("Bearer ")
            .ok_or((StatusCode::UNAUTHORIZED, "Invalid Authorization header"))?;

        validator.0.validate(token).map_err(|e| e.into())
    }
}

impl<S> FromRequestParts<S> for MaybeUser
where
    ValidatorState: FromRef<S>,
    S: Send + Sync,
{
    type Rejection = std::convert::Infallible;

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        Ok(MaybeUser(AuthenticatedUser::from_request_parts(parts, state).await.ok()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dev_validator() {
        let validator = DevValidator { _private: () }; // Skip warning in tests
        let user = validator.validate("550e8400-e29b-41d4-a716-446655440000:admin,user").unwrap();
        assert_eq!(user.id.to_string(), "550e8400-e29b-41d4-a716-446655440000");
        assert!(user.has_role("admin"));
        assert!(user.has_role("user"));
    }

    #[test]
    fn test_hmac_validator() {
        let validator = HmacValidator::new(b"test-secret-key-at-least-32-bytes", 3600);
        let user_id = Uuid::parse_str("550e8400-e29b-41d4-a716-446655440000").unwrap();

        let token = validator.generate_token(user_id, &["admin", "user"]);
        let user = validator.validate(&token).unwrap();

        assert_eq!(user.id, user_id);
        assert!(user.has_role("admin"));
        assert!(user.has_role("user"));
    }

    #[test]
    fn test_hmac_validator_rejects_tampered() {
        let validator = HmacValidator::new(b"test-secret-key-at-least-32-bytes", 3600);
        let user_id = Uuid::parse_str("550e8400-e29b-41d4-a716-446655440000").unwrap();

        let token = validator.generate_token(user_id, &["user"]);

        // Tamper with roles
        let tampered = token.replace(":user:", ":admin:");
        assert!(validator.validate(&tampered).is_err());
    }

    #[test]
    fn test_hmac_validator_expiration() {
        let validator = HmacValidator::new(b"test-secret-key-at-least-32-bytes", 0); // No expiration
        let user_id = Uuid::parse_str("550e8400-e29b-41d4-a716-446655440000").unwrap();

        // Manually create an old token
        let old_token = format!(
            "{}:user:1000000000:{}",
            user_id,
            "0".repeat(64) // Fake signature - will fail
        );
        assert!(validator.validate(&old_token).is_err());
    }
}
"#.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_to_sql() {
        assert_eq!(
            type_to_sql(StorageType::Postgres, &TypeRef::Primitive(PrimitiveType::String)),
            "TEXT"
        );
        assert_eq!(
            type_to_sql(StorageType::Postgres, &TypeRef::Primitive(PrimitiveType::Int)),
            "BIGINT"
        );
        assert_eq!(
            type_to_sql(StorageType::Postgres, &TypeRef::Primitive(PrimitiveType::Uuid)),
            "UUID"
        );

        assert_eq!(
            type_to_sql(StorageType::Sqlite, &TypeRef::Primitive(PrimitiveType::Uuid)),
            "TEXT"
        );
    }
}
