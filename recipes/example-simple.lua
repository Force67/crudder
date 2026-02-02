-- Example recipe demonstrating the improved Lua UX
-- This shows how to use templates, type mappers, and line builders

return {
    name = "example-simple",
    description = "Simple example demonstrating new UX features",
    generate = function(schema, ctx)
        -- =====================================================================
        -- Feature 1: Template interpolation with ${var}
        -- =====================================================================
        local header = helpers.template([[
// Generated from ${name} schema
// Entities: ${count}
]], {
            name = schema.dtos[1] and schema.dtos[1].name or "unknown",
            count = #schema.dtos
        })

        -- =====================================================================
        -- Feature 2: Fluent line builder API
        -- =====================================================================
        local code = helpers.lines()
            :add_raw(header)
            :blank()
            :add("pub mod types {")
            :indent()

        -- Generate structs for each DTO
        for _, dto in ipairs(schema.dtos) do
            code:blank()
                :block("pub struct " .. dto.name .. " {", "}", function(b)
                    for _, field in ipairs(dto.fields) do
                        -- Feature 3: Built-in type mappers
                        local rust_ty = helpers.rust_type(field.ty)
                        local field_name = helpers.to_snake_case(field.name)
                        b:add("pub " .. field_name .. ": " .. rust_ty .. ",")
                    end
                end)
        end

        code:dedent()
            :add("}")

        ctx:emit_file("src/types.rs", code:build())

        -- =====================================================================
        -- Feature 4: TypeScript types with ts_type()
        -- =====================================================================
        local ts = helpers.lines()
            :add("// TypeScript types")
            :blank()

        for _, dto in ipairs(schema.dtos) do
            ts:block("export interface " .. dto.name .. " {", "}", function(b)
                for _, field in ipairs(dto.fields) do
                    local ts_ty = helpers.ts_type(field.ty)
                    local field_name = helpers.to_camel_case(field.name)
                    b:add(field_name .. ": " .. ts_ty .. ";")
                end
            end)
            :blank()
        end

        ctx:emit_file("src/types.ts", ts:build())

        -- =====================================================================
        -- Feature 5: SQL types with sql_type()
        -- =====================================================================
        for _, dto in ipairs(schema.dtos) do
            if dto:is_entity() then
                local table_name = dto:table_name() or helpers.to_snake_case(dto.name)
                local sql = helpers.lines()
                    :add("-- Table: " .. table_name)
                    :blank()
                    :add("CREATE TABLE " .. table_name .. " (")
                    :indent()

                local field_lines = {}
                for _, field in ipairs(dto.fields) do
                    local col_name = helpers.to_snake_case(field.name)
                    local sql_ty = helpers.sql_type(field.ty, "postgres")
                    local constraints = {}

                    if field:is_primary() then
                        table.insert(constraints, "PRIMARY KEY")
                    end
                    if field.ty.kind ~= "optional" then
                        table.insert(constraints, "NOT NULL")
                    end

                    local line = col_name .. " " .. sql_ty
                    if #constraints > 0 then
                        line = line .. " " .. table.concat(constraints, " ")
                    end
                    table.insert(field_lines, line)
                end

                for i, line in ipairs(field_lines) do
                    local suffix = i < #field_lines and "," or ""
                    sql:add(line .. suffix)
                end

                sql:dedent()
                    :add(");")

                ctx:emit_file("migrations/create_" .. table_name .. ".sql", sql:build())
            end
        end

        ctx:log("Generated example with templates, line builder, and type mappers")
    end
}
