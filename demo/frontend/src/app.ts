import { ApiClient } from "./client";
import type { Todo } from "./types";

const client = new ApiClient({ baseUrl: "http://localhost:3000" });

const todoInput = document.getElementById("todo-input") as HTMLInputElement;
const addBtn = document.getElementById("add-btn") as HTMLButtonElement;
const todoList = document.getElementById("todo-list") as HTMLUListElement;
const errorDiv = document.getElementById("error") as HTMLDivElement;
const statsDiv = document.getElementById("stats") as HTMLDivElement;
const totalCount = document.getElementById("total-count") as HTMLSpanElement;
const completedCount = document.getElementById("completed-count") as HTMLSpanElement;

let todos: Todo[] = [];

function showError(message: string) {
  errorDiv.textContent = message;
  errorDiv.style.display = "block";
  setTimeout(() => {
    errorDiv.style.display = "none";
  }, 3000);
}

function formatDate(date: Date): string {
  return new Intl.DateTimeFormat("en-US", {
    month: "short",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit",
  }).format(date);
}

function renderTodos() {
  if (todos.length === 0) {
    todoList.innerHTML = '<li class="empty-state">No todos yet. Add one above!</li>';
    statsDiv.style.display = "none";
    return;
  }

  todoList.innerHTML = todos
    .map(
      (todo) => `
      <li class="todo-item" data-id="${todo.id}">
        <input
          type="checkbox"
          class="todo-checkbox"
          ${todo.completed ? "checked" : ""}
        />
        <span class="todo-title ${todo.completed ? "completed" : ""}">${escapeHtml(todo.title)}</span>
        <span class="todo-date">${formatDate(new Date(todo.createdAt))}</span>
        <button class="todo-delete">×</button>
      </li>
    `
    )
    .join("");

  statsDiv.style.display = "flex";
  totalCount.textContent = `${todos.length} item${todos.length !== 1 ? "s" : ""}`;
  const completed = todos.filter((t) => t.completed).length;
  completedCount.textContent = `${completed} completed`;
}

function escapeHtml(text: string): string {
  const div = document.createElement("div");
  div.textContent = text;
  return div.innerHTML;
}

async function loadTodos() {
  try {
    const response = await client.listTodos({});
    todos = response.todos;
    renderTodos();
  } catch (error) {
    showError("Failed to load todos");
    console.error(error);
    todoList.innerHTML = '<li class="empty-state">Failed to load todos</li>';
  }
}

async function addTodo() {
  const title = todoInput.value.trim();
  if (!title) return;

  try {
    const todo = await client.createTodo({ title });
    todos.push(todo);
    renderTodos();
    todoInput.value = "";
  } catch (error) {
    showError("Failed to add todo");
    console.error(error);
  }
}

async function toggleTodo(id: string, completed: boolean) {
  try {
    const updated = await client.updateTodo(id, { completed });
    const index = todos.findIndex((t) => t.id === id);
    if (index !== -1) {
      todos[index] = updated;
      renderTodos();
    }
  } catch (error) {
    showError("Failed to update todo");
    console.error(error);
  }
}

async function deleteTodo(id: string) {
  try {
    await client.deleteTodo(id);
    todos = todos.filter((t) => t.id !== id);
    renderTodos();
  } catch (error) {
    showError("Failed to delete todo");
    console.error(error);
  }
}

// Event listeners
addBtn.addEventListener("click", addTodo);

todoInput.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    addTodo();
  }
});

todoList.addEventListener("change", (e) => {
  const target = e.target as HTMLInputElement;
  if (target.classList.contains("todo-checkbox")) {
    const li = target.closest(".todo-item") as HTMLLIElement;
    const id = li.dataset.id!;
    toggleTodo(id, target.checked);
  }
});

todoList.addEventListener("click", (e) => {
  const target = e.target as HTMLElement;
  if (target.classList.contains("todo-delete")) {
    const li = target.closest(".todo-item") as HTMLLIElement;
    const id = li.dataset.id!;
    deleteTodo(id);
  }
});

// Initial load
loadTodos();
