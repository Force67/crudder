// Generated Express handlers from Crudder schema

import type { Request, Response, NextFunction } from "express";
import type * as Types from "./types";

export type Handler<TReq, TRes> = (
  req: TReq,
  res: Response,
  next: NextFunction
) => Promise<TRes | void>;

// TodoService handlers

export type listTodosHandler = Handler<
  Request<{}, Types.TodoList, Types.Empty>,
  Types.TodoList
>;

export type createTodoHandler = Handler<
  Request<{}, Types.Todo, Types.CreateTodoRequest>,
  Types.Todo
>;

export type getTodoHandler = Handler<
  Request<{}, Types.Todo, Types.Empty>,
  Types.Todo
>;

export type updateTodoHandler = Handler<
  Request<{}, Types.Todo, Types.UpdateTodoRequest>,
  Types.Todo
>;

export type deleteTodoHandler = Handler<
  Request<{}, Types.Empty, Types.Empty>,
  Types.Empty
>;

