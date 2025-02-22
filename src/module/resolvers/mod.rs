use crate::func::native::SendSync;
use crate::{Engine, Module, Position, RhaiResultOf, Shared, AST};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

mod collection;
mod dummy;
mod file;
mod stat;

pub use collection::ModuleResolversCollection;
pub use dummy::DummyModuleResolver;
#[cfg(not(feature = "no_std"))]
#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(target_arch = "wasm64"))]
pub use file::FileModuleResolver;
pub use stat::StaticModuleResolver;

/// Trait that encapsulates a module resolution service.
pub trait ModuleResolver: SendSync {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        source_path: Option<&str>,
        path: &str,
        pos: Position,
    ) -> RhaiResultOf<Shared<Module>>;

    /// Resolve an `AST` based on a path string.
    ///
    /// Returns [`None`] (default) if such resolution is not supported
    /// (e.g. if the module is Rust-based).
    ///
    /// # WARNING - Low Level API
    ///
    /// Override the default implementation of this method if the module resolver
    /// serves modules based on compiled Rhai scripts.
    #[allow(unused_variables)]
    #[must_use]
    fn resolve_ast(
        &self,
        engine: &Engine,
        source_path: Option<&str>,
        path: &str,
        pos: Position,
    ) -> Option<RhaiResultOf<AST>> {
        None
    }
}
