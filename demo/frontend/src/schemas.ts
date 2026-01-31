// Generated Zod schemas from Crudder schema

import { z } from "zod";

export const TodoSchema = z.object({
  id: z.string().uuid(),
  title: z.string(),
  completed: z.boolean(),
  createdAt: z.coerce.date(),
});
export type Todo = z.infer<typeof TodoSchema>;

export const CreateTodoRequestSchema = z.object({
  title: z.string(),
});
export type CreateTodoRequest = z.infer<typeof CreateTodoRequestSchema>;

export const UpdateTodoRequestSchema = z.object({
  title: z.string().optional(),
  completed: z.boolean().optional(),
});
export type UpdateTodoRequest = z.infer<typeof UpdateTodoRequestSchema>;

export const TodoListSchema = z.object({
  todos: TodoSchema.array(),
});
export type TodoList = z.infer<typeof TodoListSchema>;

export const EmptySchema = z.object({
});
export type Empty = z.infer<typeof EmptySchema>;

