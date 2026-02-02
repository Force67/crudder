-- TypeScript + React client recipe
-- Generates TypeScript interfaces, API client, Zod schemas, and React Query hooks

local function primitive_to_ts(primitive)
    local mapping = {
        string = "string",
        int = "number",
        float = "number",
        bool = "boolean",
        uuid = "string",
        cuid2 = "string",
        timestamp = "Date",
        bytes = "Uint8Array",
    }
    return mapping[primitive] or "unknown"
end

local function type_to_ts(ty)
    if ty.kind == "primitive" then
        return primitive_to_ts(ty.primitive)
    elseif ty.kind == "array" then
        return type_to_ts(ty.inner) .. "[]"
    elseif ty.kind == "optional" then
        return type_to_ts(ty.inner) .. " | null"
    elseif ty.kind == "named" then
        return ty.name
    end
    return "unknown"
end

local function primitive_to_zod(primitive)
    local mapping = {
        string = "z.string()",
        int = "z.number().int()",
        float = "z.number()",
        bool = "z.boolean()",
        uuid = "z.string().uuid()",
        cuid2 = "z.string()",
        timestamp = "z.coerce.date()",
        bytes = "z.instanceof(Uint8Array)",
    }
    return mapping[primitive] or "z.unknown()"
end

local function type_to_zod(ty)
    if ty.kind == "primitive" then
        return primitive_to_zod(ty.primitive)
    elseif ty.kind == "array" then
        return type_to_zod(ty.inner) .. ".array()"
    elseif ty.kind == "optional" then
        return type_to_zod(ty.inner) .. ".nullable()"
    elseif ty.kind == "named" then
        return ty.name .. "Schema"
    end
    return "z.unknown()"
end

local function generate_types(schema, ctx)
    local lines = {
        "// Generated TypeScript types from Crudder schema",
        "",
    }

    for _, dto in ipairs(schema.dtos) do
        table.insert(lines, "export interface " .. dto.name .. " {")

        for _, field in ipairs(dto.fields) do
            local ts_type = type_to_ts(field.ty)
            local optional = field.ty:is_optional() and "?" or ""
            table.insert(lines, "  " .. field.name .. optional .. ": " .. ts_type .. ";")
        end

        table.insert(lines, "}")
        table.insert(lines, "")
    end

    ctx:emit_file("src/types.ts", table.concat(lines, "\n"))
end

local function generate_zod_schemas(schema, ctx)
    local lines = {
        "// Generated Zod validation schemas from Crudder schema",
        "",
        'import { z } from "zod";',
        "",
    }

    for _, dto in ipairs(schema.dtos) do
        table.insert(lines, "export const " .. dto.name .. "Schema = z.object({")

        for _, field in ipairs(dto.fields) do
            local zod_type = type_to_zod(field.ty)
            table.insert(lines, "  " .. field.name .. ": " .. zod_type .. ",")
        end

        table.insert(lines, "});")
        table.insert(lines, "")
        table.insert(lines, "export type " .. dto.name .. " = z.infer<typeof " .. dto.name .. "Schema>;")
        table.insert(lines, "")
    end

    ctx:emit_file("src/schemas.ts", table.concat(lines, "\n"))
end

local function generate_api_client(schema, ctx)
    local api_base = ctx:option("api_base") or "/api"

    local lines = {
        "// Generated API client from Crudder schema",
        "",
        'import type * as Types from "./types";',
        "",
        "const API_BASE = " .. '"' .. api_base .. '";',
        "",
        "async function fetchJson<T>(url: string, options?: RequestInit): Promise<T> {",
        "  const response = await fetch(url, {",
        "    ...options,",
        '    headers: {',
        '      "Content-Type": "application/json",',
        "      ...options?.headers,",
        "    },",
        "  });",
        "",
        "  if (!response.ok) {",
        "    throw new Error(`HTTP error: ${response.status}`);",
        "  }",
        "",
        "  return response.json();",
        "}",
        "",
    }

    for _, service in ipairs(schema.services) do
        local service_name = helpers.to_camel_case(service.name)
        table.insert(lines, "export const " .. service_name .. " = {")

        for _, method in ipairs(service.methods) do
            local fn_name = helpers.to_camel_case(method.name)
            local http_method = method:http_method() or "GET"
            local http_path = method:http_path() or "/"

            -- Extract path parameters
            local path_params = {}
            for param in string.gmatch(http_path, "{([^}]+)}") do
                table.insert(path_params, param)
            end

            -- Build function parameters
            local params = {}
            for _, param in ipairs(path_params) do
                table.insert(params, param .. ": string")
            end

            local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH")
                and method.input ~= "Empty"

            if has_body then
                table.insert(params, "data: Types." .. method.input)
            end

            local params_str = table.concat(params, ", ")

            -- Return type
            local return_type = "void"
            if method.output and method.output ~= "Empty" then
                return_type = "Types." .. method.output
            end

            -- Build URL with template literals
            local url_template = http_path
            for _, param in ipairs(path_params) do
                url_template = string.gsub(url_template, "{" .. param .. "}", "${" .. param .. "}")
            end

            table.insert(lines, "  async " .. fn_name .. "(" .. params_str .. "): Promise<" .. return_type .. "> {")
            table.insert(lines, "    const url = `${API_BASE}" .. url_template .. "`;")

            if return_type == "void" then
                table.insert(lines, "    await fetch(url, {")
                table.insert(lines, '      method: "' .. http_method .. '",')
                if has_body then
                    table.insert(lines, '      headers: { "Content-Type": "application/json" },')
                    table.insert(lines, "      body: JSON.stringify(data),")
                end
                table.insert(lines, "    });")
            else
                if has_body then
                    table.insert(lines, "    return fetchJson<" .. return_type .. ">(url, {")
                    table.insert(lines, '      method: "' .. http_method .. '",')
                    table.insert(lines, "      body: JSON.stringify(data),")
                    table.insert(lines, "    });")
                else
                    if http_method == "GET" then
                        table.insert(lines, "    return fetchJson<" .. return_type .. ">(url);")
                    else
                        table.insert(lines, "    return fetchJson<" .. return_type .. '>(url, { method: "' .. http_method .. '" });')
                    end
                end
            end

            table.insert(lines, "  },")
            table.insert(lines, "")
        end

        table.insert(lines, "};")
        table.insert(lines, "")
    end

    ctx:emit_file("src/api.ts", table.concat(lines, "\n"))
end

local function generate_react_hooks(schema, ctx)
    local with_zod = ctx:option("with_zod") == "true"

    local lines = {
        "// Generated React Query hooks from Crudder schema",
        "",
        'import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";',
        'import type * as Types from "./types";',
    }

    -- Import API client
    local service_imports = {}
    for _, service in ipairs(schema.services) do
        table.insert(service_imports, helpers.to_camel_case(service.name))
    end
    table.insert(lines, 'import { ' .. table.concat(service_imports, ", ") .. ' } from "./api";')

    if with_zod then
        local schema_imports = {}
        for _, dto in ipairs(schema.dtos) do
            table.insert(schema_imports, dto.name .. "Schema")
        end
        table.insert(lines, 'import { ' .. table.concat(schema_imports, ", ") .. ' } from "./schemas";')
    end

    table.insert(lines, "")

    for _, service in ipairs(schema.services) do
        local service_var = helpers.to_camel_case(service.name)

        for _, method in ipairs(service.methods) do
            local fn_name = helpers.to_camel_case(method.name)
            local hook_name = "use" .. method.name
            local http_method = method:http_method() or "GET"

            -- Extract path parameters
            local path_params = {}
            for param in string.gmatch(method:http_path() or "/", "{([^}]+)}") do
                table.insert(path_params, param)
            end

            local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH")
                and method.input ~= "Empty"

            if http_method == "GET" then
                -- Query hook
                local params_type = ""
                local params_destructure = ""
                local query_key_parts = { '"' .. service.name .. '"', '"' .. method.name .. '"' }

                if #path_params > 0 then
                    local param_types = {}
                    for _, param in ipairs(path_params) do
                        table.insert(param_types, param .. ": string")
                        table.insert(query_key_parts, param)
                    end
                    params_type = "{ " .. table.concat(param_types, ", ") .. " }"
                    params_destructure = "{ " .. table.concat(path_params, ", ") .. " }"
                end

                table.insert(lines, "export function " .. hook_name .. "(" .. (params_destructure ~= "" and params_destructure .. ": " .. params_type or "") .. ") {")
                table.insert(lines, "  return useQuery({")
                table.insert(lines, "    queryKey: [" .. table.concat(query_key_parts, ", ") .. "],")

                local call_args = table.concat(path_params, ", ")
                table.insert(lines, "    queryFn: () => " .. service_var .. "." .. fn_name .. "(" .. call_args .. "),")

                table.insert(lines, "  });")
                table.insert(lines, "}")
                table.insert(lines, "")
            else
                -- Mutation hook
                table.insert(lines, "export function " .. hook_name .. "() {")
                table.insert(lines, "  const queryClient = useQueryClient();")
                table.insert(lines, "")
                table.insert(lines, "  return useMutation({")

                -- Build mutation function type
                local mutation_params = {}
                for _, param in ipairs(path_params) do
                    table.insert(mutation_params, param .. ": string")
                end
                if has_body then
                    table.insert(mutation_params, "data: Types." .. method.input)
                end

                if #mutation_params > 0 then
                    table.insert(lines, "    mutationFn: ({ " .. table.concat(path_params, ", ") .. (has_body and ", data" or "") .. " }: { " .. table.concat(mutation_params, "; ") .. " }) =>")
                    local call_args = {}
                    for _, param in ipairs(path_params) do
                        table.insert(call_args, param)
                    end
                    if has_body then
                        table.insert(call_args, "data")
                    end
                    table.insert(lines, "      " .. service_var .. "." .. fn_name .. "(" .. table.concat(call_args, ", ") .. "),")
                else
                    table.insert(lines, "    mutationFn: () => " .. service_var .. "." .. fn_name .. "(),")
                end

                table.insert(lines, "    onSuccess: () => {")
                table.insert(lines, '      queryClient.invalidateQueries({ queryKey: ["' .. service.name .. '"] });')
                table.insert(lines, "    },")
                table.insert(lines, "  });")
                table.insert(lines, "}")
                table.insert(lines, "")
            end
        end
    end

    ctx:emit_file("src/hooks.ts", table.concat(lines, "\n"))
end

return {
    name = "typescript-react",
    description = "TypeScript + React client",
    generate = function(schema, ctx)
        local with_zod = ctx:option("with_zod") == "true"

        generate_types(schema, ctx)

        if with_zod then
            generate_zod_schemas(schema, ctx)
        end

        generate_api_client(schema, ctx)
        generate_react_hooks(schema, ctx)

        ctx:log("Generated TypeScript + React client")
    end
}
