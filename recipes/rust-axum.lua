-- Rust + Axum HTTP server recipe
-- Generates a complete Rust backend with Axum routing and optional SQLx database support

local function primitive_to_rust(primitive, use_short)
    if use_short then
        local mapping = {
            string = "String",
            int = "i64",
            float = "f64",
            bool = "bool",
            uuid = "Uuid",
            cuid2 = "String",
            timestamp = "DateTime<Utc>",
            bytes = "Vec<u8>",
        }
        return mapping[primitive] or "String"
    else
        local mapping = {
            string = "String",
            int = "i64",
            float = "f64",
            bool = "bool",
            uuid = "uuid::Uuid",
            cuid2 = "String",
            timestamp = "chrono::DateTime<chrono::Utc>",
            bytes = "Vec<u8>",
        }
        return mapping[primitive] or "String"
    end
end

local function type_to_rust(ty, use_short)
    if ty.kind == "primitive" then
        return primitive_to_rust(ty.primitive, use_short)
    elseif ty.kind == "array" then
        return "Vec<" .. type_to_rust(ty.inner, use_short) .. ">"
    elseif ty.kind == "optional" then
        return "Option<" .. type_to_rust(ty.inner, use_short) .. ">"
    elseif ty.kind == "named" then
        return ty.name
    end
    return "String"
end

local function primitive_to_sql(primitive, dialect)
    if dialect == "postgres" then
        local mapping = {
            string = "TEXT",
            int = "BIGINT",
            float = "DOUBLE PRECISION",
            bool = "BOOLEAN",
            uuid = "UUID",
            cuid2 = "TEXT",
            timestamp = "TIMESTAMPTZ",
            bytes = "BYTEA",
        }
        return mapping[primitive] or "TEXT"
    else -- sqlite
        local mapping = {
            string = "TEXT",
            int = "INTEGER",
            float = "REAL",
            bool = "INTEGER",
            uuid = "TEXT",
            cuid2 = "TEXT",
            timestamp = "TEXT",
            bytes = "BLOB",
        }
        return mapping[primitive] or "TEXT"
    end
end

local function type_to_sql(ty, dialect)
    if ty.kind == "primitive" then
        return primitive_to_sql(ty.primitive, dialect)
    elseif ty.kind == "optional" then
        return type_to_sql(ty.inner, dialect)
    elseif ty.kind == "array" then
        -- Arrays as JSON in SQL
        if dialect == "postgres" then
            return "JSONB"
        else
            return "TEXT"
        end
    elseif ty.kind == "named" then
        return "JSONB" -- nested types as JSON
    end
    return "TEXT"
end

local function generate_types(schema, ctx)
    local sqlx = ctx:option("sqlx")
    local lines = {
        "//! Generated types from Crudder schema.",
        "",
        "#![allow(dead_code)]",
        "",
        "use serde::{Deserialize, Serialize};",
    }

    if sqlx then
        table.insert(lines, "use sqlx::FromRow;")
    end

    -- Check if we need uuid/chrono
    local needs_uuid = false
    local needs_chrono = false
    for _, dto in ipairs(schema.dtos) do
        for _, field in ipairs(dto.fields) do
            local base = field.ty:base_type()
            if base.primitive == "uuid" then
                needs_uuid = true
            end
            if base.primitive == "timestamp" then
                needs_chrono = true
            end
        end
    end

    if needs_uuid then
        table.insert(lines, "use uuid::Uuid;")
    end
    if needs_chrono then
        table.insert(lines, "use chrono::{DateTime, Utc};")
    end

    table.insert(lines, "")

    for _, dto in ipairs(schema.dtos) do
        -- Derive attributes
        local derives = { "Debug", "Clone", "Serialize", "Deserialize" }
        if sqlx and dto:is_entity() then
            table.insert(derives, "FromRow")
        end

        table.insert(lines, "#[derive(" .. table.concat(derives, ", ") .. ")]")
        table.insert(lines, '#[serde(rename_all = "camelCase")]')
        table.insert(lines, "pub struct " .. dto.name .. " {")

        for _, field in ipairs(dto.fields) do
            local rust_type = type_to_rust(field.ty, true)
            table.insert(lines, "    pub " .. helpers.to_snake_case(field.name) .. ": " .. rust_type .. ",")
        end

        table.insert(lines, "}")
        table.insert(lines, "")
    end

    ctx:emit_file("src/types.rs", table.concat(lines, "\n"))
end

local function generate_service_handlers(service, schema, ctx)
    local sqlx = ctx:option("sqlx")
    local service_name_snake = helpers.to_snake_case(service.name)

    -- Find the entity this service manages (if any)
    local entity = nil
    local entity_table = nil
    for _, dto in ipairs(schema.dtos) do
        if dto:is_entity() then
            entity = dto
            entity_table = dto:table_name()
            break
        end
    end

    local lines = {
        "//! " .. service.name .. " handlers.",
        "",
    }

    if not sqlx then
        table.insert(lines, "#![allow(unused_variables)]")
        table.insert(lines, "")
    end

    -- Build imports based on what's needed
    local needs_path = false
    local needs_json = false
    for _, method in ipairs(service.methods) do
        local http_path = method:http_path() or "/"
        local http_method = method:http_method() or "GET"
        if string.find(http_path, "{") then
            needs_path = true
        end
        if (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty" then
            needs_json = true
        end
    end

    local extracts = {}
    if needs_path then
        table.insert(extracts, "Path")
    end
    table.insert(extracts, "State")

    table.insert(lines, "use axum::{")
    table.insert(lines, "    extract::{" .. table.concat(extracts, ", ") .. "},")
    table.insert(lines, "    http::StatusCode,")
    table.insert(lines, "    routing::{get, post, put, delete},")
    if needs_json then
        table.insert(lines, "    Json,")
    end
    table.insert(lines, "    Router,")
    table.insert(lines, "};")

    if sqlx then
        if sqlx == "postgres" then
            table.insert(lines, "use sqlx::PgPool;")
        else
            table.insert(lines, "use sqlx::SqlitePool;")
        end
    end

    table.insert(lines, "")
    table.insert(lines, "use crate::types::*;")
    table.insert(lines, "")

    -- AppState type
    if sqlx == "postgres" then
        table.insert(lines, "pub type AppState = PgPool;")
    elseif sqlx == "sqlite" then
        table.insert(lines, "pub type AppState = SqlitePool;")
    else
        table.insert(lines, "pub type AppState = ();")
    end

    table.insert(lines, "")

    -- Router function
    table.insert(lines, "pub fn " .. service_name_snake .. "_router() -> Router<AppState> {")
    table.insert(lines, "    Router::new()")

    for _, method in ipairs(service.methods) do
        local fn_name = helpers.to_snake_case(method.name)
        local http_method = method:http_method() or "GET"
        local http_path = method:http_path() or "/"
        local axum_path = string.gsub(http_path, "{([^}]+)}", ":%1")
        local route_fn = http_method:lower()

        table.insert(lines, "        .route(\"" .. axum_path .. "\", " .. route_fn .. "(" .. fn_name .. "))")
    end

    table.insert(lines, "}")
    table.insert(lines, "")

    -- Handler functions
    for _, method in ipairs(service.methods) do
        local fn_name = helpers.to_snake_case(method.name)
        local http_method = method:http_method() or "GET"
        local http_path = method:http_path() or "/"

        -- Extract path parameters
        local path_params = {}
        for param in string.gmatch(http_path, "{([^}]+)}") do
            table.insert(path_params, param)
        end

        -- Build function signature
        local params = {}
        table.insert(params, "State(pool): State<AppState>")

        if #path_params > 0 then
            if #path_params == 1 then
                table.insert(params, "Path(" .. path_params[1] .. "): Path<String>")
            else
                table.insert(params, "Path((" .. table.concat(path_params, ", ") .. ")): Path<(" .. string.rep("String, ", #path_params):sub(1, -3) .. ")>")
            end
        end

        local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty"
        if has_body then
            table.insert(params, "Json(payload): Json<" .. method.input .. ">")
        end

        local return_type = "StatusCode"
        if method.output and method.output ~= "Empty" then
            return_type = "Result<Json<" .. method.output .. ">, StatusCode>"
        end

        table.insert(lines, "pub async fn " .. fn_name .. "(")
        for i, param in ipairs(params) do
            local suffix = i < #params and "," or ""
            table.insert(lines, "    " .. param .. suffix)
        end
        table.insert(lines, ") -> " .. return_type .. " {")

        -- Generate actual SQLx implementation if entity exists
        if sqlx and entity then
            local pk_field = entity:primary_key()
            local pk_name = pk_field and helpers.to_snake_case(pk_field.name) or "id"

            -- Determine what kind of operation this is based on HTTP method and name
            if http_method == "GET" and not string.find(http_path, "{") then
                -- List operation
                local output_dto = schema:get_dto(method.output)
                if output_dto then
                    -- Find the array field in output
                    local list_field = nil
                    for _, f in ipairs(output_dto.fields) do
                        if f.ty.kind == "array" then
                            list_field = f
                            break
                        end
                    end

                    if list_field then
                        local list_field_name = helpers.to_snake_case(list_field.name)
                        table.insert(lines, "    let " .. list_field_name .. " = sqlx::query_as::<_, " .. entity.name .. ">(")
                        table.insert(lines, '        "SELECT * FROM ' .. entity_table .. ' ORDER BY created_at DESC"')
                        table.insert(lines, "    )")
                        table.insert(lines, "    .fetch_all(&pool)")
                        table.insert(lines, "    .await")
                        table.insert(lines, "    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;")
                        table.insert(lines, "")
                        table.insert(lines, "    Ok(Json(" .. method.output .. " { " .. list_field_name .. " }))")
                    else
                        table.insert(lines, "    // TODO: Implement " .. method.name)
                        table.insert(lines, "    Err(StatusCode::NOT_IMPLEMENTED)")
                    end
                else
                    table.insert(lines, "    // TODO: Implement " .. method.name)
                    table.insert(lines, "    Err(StatusCode::NOT_IMPLEMENTED)")
                end

            elseif http_method == "GET" and #path_params > 0 then
                -- Get by ID operation
                table.insert(lines, "    let " .. pk_name .. " = " .. path_params[1] .. ".parse::<uuid::Uuid>().map_err(|_| StatusCode::BAD_REQUEST)?;")
                table.insert(lines, "")
                table.insert(lines, "    let result = sqlx::query_as::<_, " .. entity.name .. ">(")
                table.insert(lines, '        "SELECT * FROM ' .. entity_table .. ' WHERE ' .. pk_name .. ' = $1"')
                table.insert(lines, "    )")
                table.insert(lines, "    .bind(" .. pk_name .. ")")
                table.insert(lines, "    .fetch_optional(&pool)")
                table.insert(lines, "    .await")
                table.insert(lines, "    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;")
                table.insert(lines, "")
                table.insert(lines, "    result.map(Json).ok_or(StatusCode::NOT_FOUND)")

            elseif http_method == "POST" then
                -- Create operation
                local insert_fields = {}
                local insert_placeholders = {}
                local bind_calls = {}
                local placeholder_idx = 1

                for _, field in ipairs(entity.fields) do
                    if not field:is_auto() then
                        local field_snake = helpers.to_snake_case(field.name)
                        local col_name = helpers.to_snake_case(field.name)
                        table.insert(insert_fields, col_name)
                        table.insert(insert_placeholders, "$" .. placeholder_idx)
                        placeholder_idx = placeholder_idx + 1

                        -- Check if field exists in input DTO
                        local input_dto = schema:get_dto(method.input)
                        local found_in_input = false
                        if input_dto then
                            for _, input_field in ipairs(input_dto.fields) do
                                if input_field.name == field.name then
                                    found_in_input = true
                                    break
                                end
                            end
                        end

                        if found_in_input then
                            table.insert(bind_calls, "    .bind(&payload." .. field_snake .. ")")
                        elseif field.ty.kind == "primitive" and field.ty.primitive == "bool" then
                            table.insert(bind_calls, "    .bind(false)")
                        elseif field.ty.kind == "primitive" and field.ty.primitive == "uuid" then
                            table.insert(bind_calls, "    .bind(uuid::Uuid::nil()) // TODO: get from auth context")
                        else
                            table.insert(bind_calls, '    .bind("") // TODO: set ' .. field_snake)
                        end
                    end
                end

                table.insert(lines, "    let result = sqlx::query_as::<_, " .. entity.name .. ">(")
                table.insert(lines, '        "INSERT INTO ' .. entity_table .. ' (' .. table.concat(insert_fields, ", ") .. ') VALUES (' .. table.concat(insert_placeholders, ", ") .. ') RETURNING *"')
                table.insert(lines, "    )")
                for _, bind in ipairs(bind_calls) do
                    table.insert(lines, bind)
                end
                table.insert(lines, "    .fetch_one(&pool)")
                table.insert(lines, "    .await")
                table.insert(lines, "    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;")
                table.insert(lines, "")
                table.insert(lines, "    Ok(Json(result))")

            elseif http_method == "PUT" or http_method == "PATCH" then
                -- Update operation
                table.insert(lines, "    let " .. pk_name .. " = " .. path_params[1] .. ".parse::<uuid::Uuid>().map_err(|_| StatusCode::BAD_REQUEST)?;")
                table.insert(lines, "")

                -- Build dynamic update
                local input_dto = schema:get_dto(method.input)
                if input_dto then
                    -- Simple approach: update all provided fields
                    local set_clauses = {}
                    local binds = {}
                    local placeholder_idx = 1

                    for _, field in ipairs(input_dto.fields) do
                        local col_name = helpers.to_snake_case(field.name)
                        local field_snake = helpers.to_snake_case(field.name)
                        table.insert(set_clauses, col_name .. " = COALESCE($" .. placeholder_idx .. ", " .. col_name .. ")")
                        table.insert(binds, "    .bind(&payload." .. field_snake .. ")")
                        placeholder_idx = placeholder_idx + 1
                    end

                    table.insert(lines, "    let result = sqlx::query_as::<_, " .. entity.name .. ">(")
                    table.insert(lines, '        "UPDATE ' .. entity_table .. ' SET ' .. table.concat(set_clauses, ", ") .. ' WHERE ' .. pk_name .. ' = $' .. placeholder_idx .. ' RETURNING *"')
                    table.insert(lines, "    )")
                    for _, bind in ipairs(binds) do
                        table.insert(lines, bind)
                    end
                    table.insert(lines, "    .bind(" .. pk_name .. ")")
                    table.insert(lines, "    .fetch_optional(&pool)")
                    table.insert(lines, "    .await")
                    table.insert(lines, "    .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;")
                    table.insert(lines, "")
                    table.insert(lines, "    result.map(Json).ok_or(StatusCode::NOT_FOUND)")
                else
                    table.insert(lines, "    // TODO: Implement " .. method.name)
                    table.insert(lines, "    Err(StatusCode::NOT_IMPLEMENTED)")
                end

            elseif http_method == "DELETE" then
                -- Delete operation
                table.insert(lines, "    let " .. pk_name .. " = match " .. path_params[1] .. ".parse::<uuid::Uuid>() {")
                table.insert(lines, "        Ok(id) => id,")
                table.insert(lines, "        Err(_) => return StatusCode::BAD_REQUEST,")
                table.insert(lines, "    };")
                table.insert(lines, "")
                table.insert(lines, "    let result = match sqlx::query(")
                table.insert(lines, '        "DELETE FROM ' .. entity_table .. ' WHERE ' .. pk_name .. ' = $1"')
                table.insert(lines, "    )")
                table.insert(lines, "    .bind(" .. pk_name .. ")")
                table.insert(lines, "    .execute(&pool)")
                table.insert(lines, "    .await {")
                table.insert(lines, "        Ok(r) => r,")
                table.insert(lines, "        Err(_) => return StatusCode::INTERNAL_SERVER_ERROR,")
                table.insert(lines, "    };")
                table.insert(lines, "")
                table.insert(lines, "    if result.rows_affected() > 0 {")
                table.insert(lines, "        StatusCode::NO_CONTENT")
                table.insert(lines, "    } else {")
                table.insert(lines, "        StatusCode::NOT_FOUND")
                table.insert(lines, "    }")
            else
                table.insert(lines, "    // TODO: Implement " .. method.name)
                if method.output and method.output ~= "Empty" then
                    table.insert(lines, "    Err(StatusCode::NOT_IMPLEMENTED)")
                else
                    table.insert(lines, "    StatusCode::NOT_IMPLEMENTED")
                end
            end
        else
            -- No SQLx - placeholder implementation
            table.insert(lines, "    // TODO: Implement " .. method.name)
            if method.output and method.output ~= "Empty" then
                table.insert(lines, "    Err(StatusCode::NOT_IMPLEMENTED)")
            else
                table.insert(lines, "    StatusCode::NOT_IMPLEMENTED")
            end
        end

        table.insert(lines, "}")
        table.insert(lines, "")
    end

    ctx:emit_file("src/" .. service_name_snake .. ".rs", table.concat(lines, "\n"))
end

local function generate_migrations(schema, ctx)
    local sqlx = ctx:option("sqlx")
    if not sqlx then return end

    local migration_num = 1
    for _, dto in ipairs(schema.dtos) do
        if dto:is_entity() then
            local table_name = dto:table_name()
            local lines = {
                "-- Migration: Create " .. table_name .. " table",
                "",
                "CREATE TABLE IF NOT EXISTS " .. table_name .. " (",
            }

            local field_lines = {}

            for _, field in ipairs(dto.fields) do
                local col_name = helpers.to_snake_case(field.name)
                local sql_type = type_to_sql(field.ty, sqlx)
                local constraints = {}

                if field:is_primary() then
                    table.insert(constraints, "PRIMARY KEY")
                end

                if field:is_auto() then
                    if field.ty.kind == "primitive" and field.ty.primitive == "uuid" then
                        table.insert(constraints, "DEFAULT gen_random_uuid()")
                    elseif field.ty.kind == "primitive" and field.ty.primitive == "timestamp" then
                        table.insert(constraints, "DEFAULT NOW()")
                    end
                end

                if field.ty.kind ~= "optional" and not field:is_auto() then
                    table.insert(constraints, "NOT NULL")
                end

                local constraint_str = ""
                if #constraints > 0 then
                    constraint_str = " " .. table.concat(constraints, " ")
                end

                table.insert(field_lines, "    " .. col_name .. " " .. sql_type .. constraint_str)
            end

            table.insert(lines, table.concat(field_lines, ",\n"))
            table.insert(lines, ");")
            table.insert(lines, "")

            local filename = string.format("migrations/%03d_create_%s.sql", migration_num, table_name)
            ctx:emit_file(filename, table.concat(lines, "\n"))
            migration_num = migration_num + 1
        end
    end
end

local function generate_lib(schema, ctx)
    local sqlx = ctx:option("sqlx")

    local lines = {
        "//! Generated Axum application.",
        "",
        "pub mod types;",
    }

    for _, service in ipairs(schema.services) do
        local mod_name = helpers.to_snake_case(service.name)
        table.insert(lines, "pub mod " .. mod_name .. ";")
    end

    table.insert(lines, "")
    table.insert(lines, "use axum::Router;")

    -- Re-export AppState and router from first service
    if #schema.services > 0 then
        local first_service = helpers.to_snake_case(schema.services[1].name)
        table.insert(lines, "")
        table.insert(lines, "pub use " .. first_service .. "::{" .. first_service .. "_router, AppState};")
        table.insert(lines, "")
        table.insert(lines, "/// Creates the application router with all services.")
        table.insert(lines, "pub fn router() -> Router<AppState> {")
        table.insert(lines, "    Router::new()")

        for _, service in ipairs(schema.services) do
            local mod_name = helpers.to_snake_case(service.name)
            table.insert(lines, "        .merge(" .. mod_name .. "::" .. mod_name .. "_router())")
        end

        table.insert(lines, "}")
    end

    table.insert(lines, "")

    ctx:emit_file("src/lib.rs", table.concat(lines, "\n"))
end

local function generate_cargo_toml(schema, ctx)
    local sqlx = ctx:option("sqlx")

    local needs_uuid = false
    local needs_chrono = false
    for _, dto in ipairs(schema.dtos) do
        for _, field in ipairs(dto.fields) do
            local base = field.ty:base_type()
            if base.primitive == "uuid" then
                needs_uuid = true
            end
            if base.primitive == "timestamp" then
                needs_chrono = true
            end
        end
    end

    local lines = {
        '[package]',
        'name = "generated-server"',
        'version = "0.1.0"',
        'edition = "2021"',
        '',
        '[dependencies]',
        'axum = "0.7"',
        'serde = { version = "1", features = ["derive"] }',
        'serde_json = "1"',
        'tokio = { version = "1", features = ["full"] }',
    }

    if needs_uuid then
        table.insert(lines, 'uuid = { version = "1", features = ["v4", "serde"] }')
    end
    if needs_chrono then
        table.insert(lines, 'chrono = { version = "0.4", features = ["serde"] }')
    end

    if sqlx == "postgres" then
        table.insert(lines, 'sqlx = { version = "0.8", features = ["runtime-tokio", "postgres", "uuid", "chrono"] }')
    elseif sqlx == "sqlite" then
        table.insert(lines, 'sqlx = { version = "0.8", features = ["runtime-tokio", "sqlite", "uuid", "chrono"] }')
    end

    table.insert(lines, '')

    ctx:emit_file("Cargo.toml", table.concat(lines, "\n"))
end

return {
    name = "rust-axum",
    description = "Rust + Axum HTTP server",
    generate = function(schema, ctx)
        generate_types(schema, ctx)

        for _, service in ipairs(schema.services) do
            generate_service_handlers(service, schema, ctx)
        end

        generate_migrations(schema, ctx)
        generate_lib(schema, ctx)
        generate_cargo_toml(schema, ctx)

        ctx:log("Generated Rust + Axum project")
    end
}
