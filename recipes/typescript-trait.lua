-- TypeScript trait-based recipe
-- Generates a service interface that users implement with custom logic
-- Works with Express, Hono, or any framework

-- Helper to convert type to Zod schema
local function type_to_zod(ty)
    if ty.kind == "primitive" then
        local mapping = {
            string = "z.string()",
            int = "z.number().int()",
            float = "z.number()",
            bool = "z.boolean()",
            uuid = "z.string().uuid()",
            cuid2 = "z.string()",
            timestamp = "z.string().datetime()",
            bytes = "z.instanceof(Uint8Array)",
        }
        return mapping[ty.primitive] or "z.unknown()"
    elseif ty.kind == "array" then
        return type_to_zod(ty.inner) .. ".array()"
    elseif ty.kind == "optional" then
        return type_to_zod(ty.inner) .. ".nullable()"
    elseif ty.kind == "named" then
        return ty.name .. "Schema"
    end
    return "z.unknown()"
end

-- Generate Express router
local function generate_express_router(code, schema, with_zod)
    code:add("// =============================================================================")
    code:add("// Express Router")
    code:add("// =============================================================================")
    code:blank()
    code:add('import { Router, Request, Response, NextFunction } from "express";')
    code:blank()

    for _, service in ipairs(schema.services) do
        local interface_name = service.name
        local router_fn_name = "create" .. service.name .. "Router"

        code:add("/**")
        code:add(" * Creates an Express router from your service implementation.")
        code:add(" * @param service Your implementation of " .. interface_name)
        code:add(" */")
        code:block("export function " .. router_fn_name .. "(service: " .. interface_name .. "): Router {", "}", function(r)
            r:add("const router = Router();")
            r:blank()

            for _, method in ipairs(service.methods) do
                local fn_name = helpers.to_camel_case(method.name)
                local http_method = method:http_method() or "GET"
                local http_path = method:http_path() or "/"
                local express_path = string.gsub(http_path, "{([^}]+)}", ":%1")

                local path_params = {}
                for param in string.gmatch(http_path, "{([^}]+)}") do
                    table.insert(path_params, param)
                end

                local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty"

                r:add('router.' .. http_method:lower() .. '("' .. express_path .. '", async (req: Request, res: Response, next: NextFunction) => {')
                r:indent()
                r:add("try {")
                r:indent()

                if with_zod and has_body then
                    r:add("const request = " .. method.input .. "Schema.parse(req.body);")
                end

                local call_args = {}
                for _, p in ipairs(path_params) do
                    table.insert(call_args, "req.params." .. p .. " as string")
                end
                if has_body then
                    if with_zod then
                        table.insert(call_args, "request")
                    else
                        table.insert(call_args, "req.body as " .. method.input)
                    end
                end

                local call = "await service." .. fn_name .. "(" .. table.concat(call_args, ", ") .. ")"

                if method.output and method.output ~= "Empty" then
                    r:add("const result = " .. call .. ";")
                    r:add("res.json(result);")
                else
                    r:add(call .. ";")
                    if http_method == "DELETE" then
                        r:add("res.status(204).end();")
                    else
                        r:add("res.status(200).end();")
                    end
                end

                r:dedent()
                r:add("} catch (error) {")
                r:indent()
                r:add("next(error);")
                r:dedent()
                r:add("}")
                r:dedent()
                r:add("});")
                r:blank()
            end

            r:add("return router;")
        end)
        code:blank()
    end
end

-- Generate Hono router
local function generate_hono_router(code, schema, with_zod)
    code:add("// =============================================================================")
    code:add("// Hono Router")
    code:add("// =============================================================================")
    code:blank()
    code:add('import { Hono } from "hono";')
    code:blank()

    for _, service in ipairs(schema.services) do
        local interface_name = service.name
        local router_fn_name = "create" .. service.name .. "Router"

        code:add("/**")
        code:add(" * Creates a Hono app from your service implementation.")
        code:add(" * @param service Your implementation of " .. interface_name)
        code:add(" */")
        code:block("export function " .. router_fn_name .. "(service: " .. interface_name .. "): Hono {", "}", function(r)
            r:add("const app = new Hono();")
            r:blank()

            for _, method in ipairs(service.methods) do
                local fn_name = helpers.to_camel_case(method.name)
                local http_method = method:http_method() or "GET"
                local http_path = method:http_path() or "/"
                local hono_path = string.gsub(http_path, "{([^}]+)}", ":%1")

                local path_params = {}
                for param in string.gmatch(http_path, "{([^}]+)}") do
                    table.insert(path_params, param)
                end

                local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty"

                r:add('app.' .. http_method:lower() .. '("' .. hono_path .. '", async (c) => {')
                r:indent()

                if has_body then
                    if with_zod then
                        r:add("const body = await c.req.json();")
                        r:add("const request = " .. method.input .. "Schema.parse(body);")
                    else
                        r:add("const request = await c.req.json() as " .. method.input .. ";")
                    end
                end

                local call_args = {}
                for _, p in ipairs(path_params) do
                    table.insert(call_args, 'c.req.param("' .. p .. '")')
                end
                if has_body then
                    table.insert(call_args, "request")
                end

                local call = "await service." .. fn_name .. "(" .. table.concat(call_args, ", ") .. ")"

                if method.output and method.output ~= "Empty" then
                    r:add("const result = " .. call .. ";")
                    r:add("return c.json(result);")
                else
                    r:add(call .. ";")
                    if http_method == "DELETE" then
                        r:add("return c.body(null, 204);")
                    else
                        r:add("return c.body(null, 200);")
                    end
                end

                r:dedent()
                r:add("});")
                r:blank()
            end

            r:add("return app;")
        end)
        code:blank()
    end
end

-- Generate standalone handlers
local function generate_standalone_handlers(code, schema, with_zod)
    code:add("// =============================================================================")
    code:add("// Standalone Handlers (framework-agnostic)")
    code:add("// =============================================================================")
    code:blank()
    code:add("export interface HttpRequest {")
    code:add("  params: Record<string, string>;")
    code:add("  body: unknown;")
    code:add("}")
    code:blank()
    code:add("export interface HttpResponse<T = unknown> {")
    code:add("  status: number;")
    code:add("  body?: T;")
    code:add("}")
    code:blank()

    for _, service in ipairs(schema.services) do
        local interface_name = service.name
        local handlers_name = service.name .. "Handlers"

        code:add("/**")
        code:add(" * Creates handler functions from your service implementation.")
        code:add(" */")
        code:block("export function create" .. handlers_name .. "(service: " .. interface_name .. ") {", "}", function(r)
            r:add("return {")
            r:indent()

            for i, method in ipairs(service.methods) do
                local fn_name = helpers.to_camel_case(method.name)
                local http_method = method:http_method() or "GET"
                local http_path = method:http_path() or "/"

                local path_params = {}
                for param in string.gmatch(http_path, "{([^}]+)}") do
                    table.insert(path_params, param)
                end

                local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty"

                local return_type = "HttpResponse"
                if method.output and method.output ~= "Empty" then
                    return_type = "HttpResponse<" .. method.output .. ">"
                end

                r:add(fn_name .. ": async (req: HttpRequest): Promise<" .. return_type .. "> => {")
                r:indent()

                local call_args = {}
                for _, p in ipairs(path_params) do
                    table.insert(call_args, 'req.params["' .. p .. '"]')
                end
                if has_body then
                    if with_zod then
                        r:add("const request = " .. method.input .. "Schema.parse(req.body);")
                        table.insert(call_args, "request")
                    else
                        table.insert(call_args, "req.body as " .. method.input)
                    end
                end

                local call = "await service." .. fn_name .. "(" .. table.concat(call_args, ", ") .. ")"

                if method.output and method.output ~= "Empty" then
                    r:add("const result = " .. call .. ";")
                    r:add("return { status: 200, body: result };")
                else
                    r:add(call .. ";")
                    if http_method == "DELETE" then
                        r:add("return { status: 204 };")
                    else
                        r:add("return { status: 200 };")
                    end
                end

                r:dedent()
                local comma = i < #service.methods and "," or ""
                r:add("}" .. comma)
            end

            r:dedent()
            r:add("};")
        end)
        code:blank()
    end
end

return {
    name = "typescript-trait",
    description = "TypeScript service interface (user implements handlers)",
    generate = function(schema, ctx)
        local framework = ctx:option("framework") or "express"
        local with_zod = ctx:option("zod") == "true"

        local code = helpers.lines()
            :add("// Generated by crudder. Do not edit.")
            :add("//")
            :add("// Implement the generated interface to add custom handler logic:")
            :add("//")
            :add("// class MyTodoService implements TodoService {")
            :add("//   async listTodos(): Promise<TodoList> {")
            :add("//     // your custom logic here")
            :add("//   }")
            :add("// }")
            :add("//")
            :add("// const router = createTodoServiceRouter(new MyTodoService());")
            :blank()

        -- Types
        code:add("// =============================================================================")
        code:add("// Types")
        code:add("// =============================================================================")
        code:blank()

        for _, dto in ipairs(schema.dtos) do
            code:block("export interface " .. dto.name .. " {", "}", function(b)
                for _, field in ipairs(dto.fields) do
                    local ts_ty = helpers.ts_type(field.ty)
                    local field_name = helpers.to_camel_case(field.name)
                    b:add(field_name .. ": " .. ts_ty .. ";")
                end
            end)
            code:blank()
        end

        -- Zod schemas
        if with_zod then
            code:add("// =============================================================================")
            code:add("// Zod Schemas")
            code:add("// =============================================================================")
            code:blank()
            code:add('import { z } from "zod";')
            code:blank()

            for _, dto in ipairs(schema.dtos) do
                local schema_name = dto.name .. "Schema"
                code:add("export const " .. schema_name .. " = z.object({")
                code:indent()

                for _, field in ipairs(dto.fields) do
                    local field_name = helpers.to_camel_case(field.name)
                    local zod_type = type_to_zod(field.ty)
                    code:add(field_name .. ": " .. zod_type .. ",")
                end

                code:dedent()
                code:add("});")
                code:blank()
            end
        end

        -- Service Interface
        code:add("// =============================================================================")
        code:add("// Service Interface - Implement this with your custom logic")
        code:add("// =============================================================================")
        code:blank()

        for _, service in ipairs(schema.services) do
            local interface_name = service.name

            code:block("export interface " .. interface_name .. " {", "}", function(t)
                for _, method in ipairs(service.methods) do
                    local fn_name = helpers.to_camel_case(method.name)
                    local http_method = method:http_method() or "GET"
                    local http_path = method:http_path() or "/"

                    local path_params = {}
                    for param in string.gmatch(http_path, "{([^}]+)}") do
                        table.insert(path_params, param)
                    end

                    local params = {}
                    for _, p in ipairs(path_params) do
                        table.insert(params, p .. ": string")
                    end

                    local has_body = (http_method == "POST" or http_method == "PUT" or http_method == "PATCH") and method.input ~= "Empty"
                    if has_body then
                        table.insert(params, "request: " .. method.input)
                    end

                    local return_type = "void"
                    if method.output and method.output ~= "Empty" then
                        return_type = method.output
                    end

                    t:add("/** " .. http_method .. " " .. http_path .. " */")
                    t:add(fn_name .. "(" .. table.concat(params, ", ") .. "): Promise<" .. return_type .. ">;")
                    t:blank()
                end
            end)
            code:blank()
        end

        -- Framework router
        if framework == "express" then
            generate_express_router(code, schema, with_zod)
        elseif framework == "hono" then
            generate_hono_router(code, schema, with_zod)
        else
            generate_standalone_handlers(code, schema, with_zod)
        end

        ctx:emit_file("generated.ts", code:build())
        ctx:log("Generated TypeScript trait-based module for " .. framework)
    end
}
