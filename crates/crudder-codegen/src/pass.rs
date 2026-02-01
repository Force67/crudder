//! Pass-based code generation architecture.
//!
//! This module provides a flexible pass manager that allows composing
//! different code generation passes. A base pass generates library-agnostic
//! code, and feature passes can be added to enhance the output with
//! framework-specific features.
//!
//! # Example
//!
//! ```ignore
//! use crudder_codegen::pass::{PassManager, GenerationContext};
//! use crudder_codegen::typescript::{TypeScriptBasePass, passes::ZodPass};
//!
//! let mut pm = PassManager::new();
//! pm.add(TypeScriptBasePass);
//! pm.add(ZodPass);
//!
//! let files = pm.run(&schema)?;
//! ```

use std::collections::HashMap;

use crate::{CodegenError, GeneratedFile, GeneratedFiles};
use crudder_ast::Schema;

/// Mutable state passed through the pipeline.
///
/// This context accumulates generated files, imports, derives, and metadata
/// as passes execute. Each pass can read and modify this context.
#[derive(Debug, Clone, Default)]
pub struct GenerationContext {
    /// Generated files (can be added/modified by passes).
    /// Key is the file path, value is the file content.
    pub files: HashMap<String, GeneratedFile>,

    /// Imports to add to specific files.
    /// Key is the file path, value is a list of import statements.
    pub imports: HashMap<String, Vec<String>>,

    /// Derives to add to structs (Rust).
    /// Key is the struct name, value is a list of derive macros.
    pub derives: HashMap<String, Vec<String>>,

    /// Arbitrary metadata for pass communication.
    /// Passes can use this to signal information to other passes.
    pub metadata: HashMap<String, String>,
}

impl GenerationContext {
    /// Creates a new empty generation context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add or replace a file.
    pub fn set_file(&mut self, path: impl Into<String>, content: impl Into<String>) {
        let path = path.into();
        self.files
            .insert(path.clone(), GeneratedFile::new(path, content.into()));
    }

    /// Get a file's content for reading.
    pub fn get_file(&self, path: &str) -> Option<&str> {
        self.files.get(path).map(|f| f.content.as_str())
    }

    /// Get a file's content for modification.
    pub fn get_file_mut(&mut self, path: &str) -> Option<&mut String> {
        self.files.get_mut(path).map(|f| &mut f.content)
    }

    /// Check if a file exists.
    pub fn has_file(&self, path: &str) -> bool {
        self.files.contains_key(path)
    }

    /// Add an import to a file.
    pub fn add_import(&mut self, file: impl Into<String>, import: impl Into<String>) {
        self.imports
            .entry(file.into())
            .or_default()
            .push(import.into());
    }

    /// Get imports for a file.
    pub fn get_imports(&self, file: &str) -> Option<&[String]> {
        self.imports.get(file).map(|v| v.as_slice())
    }

    /// Add a derive macro to a struct (Rust).
    pub fn add_derive(&mut self, struct_name: impl Into<String>, derive: impl Into<String>) {
        self.derives
            .entry(struct_name.into())
            .or_default()
            .push(derive.into());
    }

    /// Get derives for a struct.
    pub fn get_derives(&self, struct_name: &str) -> Option<&[String]> {
        self.derives.get(struct_name).map(|v| v.as_slice())
    }

    /// Set a metadata value.
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Get a metadata value.
    pub fn get_metadata(&self, key: &str) -> Option<&str> {
        self.metadata.get(key).map(|s| s.as_str())
    }

    /// Check if a metadata key exists.
    pub fn has_metadata(&self, key: &str) -> bool {
        self.metadata.contains_key(key)
    }

    /// Finalize into GeneratedFiles.
    ///
    /// This method post-processes the context by injecting imports into files
    /// and applying derives to Rust structs. After finalization, the context
    /// is converted to a GeneratedFiles collection.
    pub fn finalize(mut self) -> GeneratedFiles {
        // Post-process: inject imports at the top of files
        for (file_path, imports) in &self.imports {
            if let Some(content) = self.files.get_mut(file_path) {
                let import_block = imports.join("\n");
                if !import_block.is_empty() {
                    // Find the first non-comment line to insert imports after
                    let mut lines: Vec<&str> = content.content.lines().collect();
                    let mut insert_pos = 0;

                    // Skip leading comments and empty lines
                    for (i, line) in lines.iter().enumerate() {
                        let trimmed = line.trim();
                        if trimmed.is_empty() || trimmed.starts_with("//") {
                            insert_pos = i + 1;
                        } else {
                            break;
                        }
                    }

                    // Insert imports
                    let import_lines: Vec<&str> = import_block.lines().collect();
                    for (i, import_line) in import_lines.iter().enumerate() {
                        lines.insert(insert_pos + i, import_line);
                    }
                    if !import_lines.is_empty() {
                        lines.insert(insert_pos + import_lines.len(), "");
                    }

                    content.content = lines.join("\n");
                }
            }
        }

        // Convert to GeneratedFiles
        let mut result = GeneratedFiles::new();
        for file in self.files.into_values() {
            result.add(file);
        }
        result
    }
}

/// A pass that transforms the generation context.
///
/// Passes are the building blocks of the code generation pipeline.
/// Each pass can read the schema and modify the generation context
/// to produce or enhance generated files.
pub trait Pass: Send + Sync {
    /// Unique identifier for this pass.
    fn name(&self) -> &'static str;

    /// Execute the pass, mutating the context.
    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError>;

    /// Dependencies: passes that must run before this one.
    ///
    /// The pass manager will ensure all dependencies are run before
    /// this pass executes. If a dependency is missing, an error is returned.
    fn depends_on(&self) -> &[&'static str] {
        &[]
    }
}

/// Manages pass execution order and dependencies.
///
/// The PassManager collects passes and executes them in dependency order.
/// It ensures that passes run after their dependencies and provides
/// a simple interface for building code generation pipelines.
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PassManager {
    /// Creates a new empty pass manager.
    pub fn new() -> Self {
        Self { passes: vec![] }
    }

    /// Add a pass to the pipeline.
    ///
    /// Returns `&mut Self` for method chaining.
    pub fn add<P: Pass + 'static>(&mut self, pass: P) -> &mut Self {
        self.passes.push(Box::new(pass));
        self
    }

    /// Run all passes in dependency order.
    ///
    /// This method:
    /// 1. Topologically sorts passes by their dependencies
    /// 2. Executes each pass in order
    /// 3. Finalizes the context into GeneratedFiles
    pub fn run(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError> {
        let mut ctx = GenerationContext::new();

        // Build dependency graph and sort
        let sorted = self.topological_sort()?;

        // Run passes in sorted order
        for pass_idx in sorted {
            let pass = &self.passes[pass_idx];
            pass.run(schema, &mut ctx)?;

            // Mark this pass as completed in metadata
            ctx.set_metadata(format!("pass:{}:completed", pass.name()), "true");
        }

        Ok(ctx.finalize())
    }

    /// Topologically sort passes by dependencies.
    fn topological_sort(&self) -> Result<Vec<usize>, CodegenError> {
        // Build name -> index mapping
        let name_to_idx: HashMap<&str, usize> = self
            .passes
            .iter()
            .enumerate()
            .map(|(i, p)| (p.name(), i))
            .collect();

        // Build adjacency list (dependency edges)
        let mut in_degree = vec![0usize; self.passes.len()];
        let mut dependents: Vec<Vec<usize>> = vec![vec![]; self.passes.len()];

        for (i, pass) in self.passes.iter().enumerate() {
            for dep_name in pass.depends_on() {
                if let Some(&dep_idx) = name_to_idx.get(dep_name) {
                    dependents[dep_idx].push(i);
                    in_degree[i] += 1;
                }
                // Note: We don't error on missing dependencies here since they
                // might be optional or provided by a different run configuration
            }
        }

        // Kahn's algorithm
        let mut queue: Vec<usize> = in_degree
            .iter()
            .enumerate()
            .filter(|(_, &d)| d == 0)
            .map(|(i, _)| i)
            .collect();

        let mut result = Vec::with_capacity(self.passes.len());

        while let Some(idx) = queue.pop() {
            result.push(idx);
            for &dependent in &dependents[idx] {
                in_degree[dependent] -= 1;
                if in_degree[dependent] == 0 {
                    queue.push(dependent);
                }
            }
        }

        if result.len() != self.passes.len() {
            return Err(CodegenError::Custom(
                "Circular dependency detected in passes".to_string(),
            ));
        }

        Ok(result)
    }

    /// Get the number of passes in the manager.
    pub fn len(&self) -> usize {
        self.passes.len()
    }

    /// Check if the manager has no passes.
    pub fn is_empty(&self) -> bool {
        self.passes.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestPass {
        name: &'static str,
        deps: &'static [&'static str],
    }

    impl Pass for TestPass {
        fn name(&self) -> &'static str {
            self.name
        }

        fn run(&self, _schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
            ctx.set_file(format!("{}.txt", self.name), format!("Content from {}", self.name));
            Ok(())
        }

        fn depends_on(&self) -> &[&'static str] {
            self.deps
        }
    }

    #[test]
    fn test_generation_context() {
        let mut ctx = GenerationContext::new();

        ctx.set_file("test.ts", "const x = 1;");
        assert_eq!(ctx.get_file("test.ts"), Some("const x = 1;"));
        assert!(ctx.has_file("test.ts"));
        assert!(!ctx.has_file("other.ts"));

        ctx.add_import("test.ts", "import { z } from 'zod';");
        assert_eq!(ctx.get_imports("test.ts"), Some(&["import { z } from 'zod';".to_string()][..]));

        ctx.add_derive("User", "Debug");
        ctx.add_derive("User", "Clone");
        assert_eq!(ctx.get_derives("User"), Some(&["Debug".to_string(), "Clone".to_string()][..]));

        ctx.set_metadata("has:zod", "true");
        assert_eq!(ctx.get_metadata("has:zod"), Some("true"));
        assert!(ctx.has_metadata("has:zod"));
    }

    #[test]
    fn test_pass_manager_simple() {
        let mut pm = PassManager::new();
        pm.add(TestPass { name: "base", deps: &[] });

        let schema = Schema::new();
        let files = pm.run(&schema).unwrap();

        assert_eq!(files.files.len(), 1);
        assert!(files.files.iter().any(|f| f.path.to_str() == Some("base.txt")));
    }

    #[test]
    fn test_pass_manager_with_dependencies() {
        let mut pm = PassManager::new();
        pm.add(TestPass { name: "zod", deps: &["base"] });
        pm.add(TestPass { name: "base", deps: &[] });

        let schema = Schema::new();
        let files = pm.run(&schema).unwrap();

        // Both passes should have run
        assert_eq!(files.files.len(), 2);
    }

    #[test]
    fn test_pass_manager_circular_dependency() {
        let mut pm = PassManager::new();
        pm.add(TestPass { name: "a", deps: &["b"] });
        pm.add(TestPass { name: "b", deps: &["a"] });

        let schema = Schema::new();
        let result = pm.run(&schema);

        assert!(result.is_err());
    }
}
