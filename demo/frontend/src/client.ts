// Generated API client from Crudder schema

import type * as Types from "./types";

export interface ClientOptions {
  baseUrl: string;
  fetch?: typeof fetch;
}

export class ApiClient {
  private baseUrl: string;
  private fetch: typeof fetch;

  constructor(options: ClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, "");
    this.fetch = options.fetch ?? globalThis.fetch;
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown
  ): Promise<T> {
    const response = await this.fetch(`${this.baseUrl}${path}`, {
      method,
      headers: {
        "Content-Type": "application/json",
      },
      body: body ? JSON.stringify(body) : undefined,
    });

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }

    return response.json();
  }

  async listTodos(request: Types.Empty): Promise<Types.TodoList> {
    const params = new URLSearchParams(request as Record<string, string>);
    return this.request("GET", `/todos?${params}`);
  }

  async createTodo(request: Types.CreateTodoRequest): Promise<Types.Todo> {
    return this.request("POST", "/todos", request);
  }

  async getTodo(id: string): Promise<Types.Todo> {
    return this.request("GET", `/todos/${id}`);
  }

  async updateTodo(id: string, request: Types.UpdateTodoRequest): Promise<Types.Todo> {
    return this.request("PUT", `/todos/${id}`, request);
  }

  async deleteTodo(id: string): Promise<Types.Empty> {
    return this.request("DELETE", `/todos/${id}`);
  }

}
