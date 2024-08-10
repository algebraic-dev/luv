//! Module providing a generic [Id] type to identify entities within a [Storage].
//!
//! The [Id<T>] type is parameterized by a [TypeKind] trait, which acts as a marker for the different
//! types that can be identified.

use core::fmt;
use std::marker::PhantomData;

/// Macro to define new `Id` types.
macro_rules! define_id_type {
    ($name:ident, $doc:expr) => {
        #[doc = $doc]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name;
        impl sealed::Sealed for $name {}
        impl TypeKind for $name {}
    };
}

/// Sealed trait pattern to prevent external implementations of [TypeKind[.
mod sealed {
    pub trait Sealed {}
}

/// Trait used to mark types that can be identified by an `Id`.
pub trait TypeKind: sealed::Sealed {}

/// Generic identifier type for entities in a [Storage].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id<T: TypeKind> {
    value: u64,
    marker: PhantomData<T>,
}

impl<T: TypeKind> fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id({})", self.value)
    }
}

impl<T: TypeKind> Id<T> {
    /// Creates a new `Id` with the given value.
    pub fn new(value: u64) -> Self {
        Id {
            value,
            marker: PhantomData,
        }
    }

    /// Returns the value of the `Id`.
    pub fn value(&self) -> u64 {
        self.value
    }
}

// Section for defining all the Id types that are used in the environment.

define_id_type!(Evaluated, "An identifier for evaluated entities.");

define_id_type!(Module, "An identifier for modules.");

define_id_type!(File, "An identifier for files.");

define_id_type!(Definition, "An identifier for definitions.");

/// Anonymous Id stuff.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnId {
    Evaluated(Id<Evaluated>),
    Module(Id<Module>),
    File(Id<File>),
    Definition(Id<Definition>),
}

impl fmt::Debug for AnId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnId::Evaluated(id) => write!(f, "Evaluated({})",id.value),
            AnId::Module(id) => write!(f, "Module({})",id.value),
            AnId::File(id) => write!(f, "File({})",id.value),
            AnId::Definition(id) => write!(f, "Definition({})",id.value),
        }
    }
}

impl From<Id<Evaluated>> for AnId {
    fn from(value: Id<Evaluated>) -> Self {
        AnId::Evaluated(value)
    }
}

impl From<Id<Module>> for AnId {
    fn from(value: Id<Module>) -> Self {
        AnId::Module(value)
    }
}

impl From<Id<File>> for AnId {
    fn from(value: Id<File>) -> Self {
        AnId::File(value)
    }
}

impl From<Id<Definition>> for AnId {
    fn from(value: Id<Definition>) -> Self {
        AnId::Definition(value)
    }
}
