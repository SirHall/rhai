use crate::def_package;
use crate::plugin::*;
use crate::types::dynamic::Tag;
use crate::{Dynamic, RhaiResultOf, ERR, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

def_package! {
    /// Package of core language features.
    crate::LanguageCorePackage => |lib| {
        lib.standard = true;

        combine_with_exported_module!(lib, "language_core", core_functions);
    }
}

#[export_module]
mod core_functions {
    #[rhai_fn(name = "!")]
    pub fn not(x: bool) -> bool {
        !x
    }
    #[rhai_fn(name = "tag", get = "tag", pure)]
    pub fn get_tag(value: &mut Dynamic) -> INT {
        value.tag() as INT
    }
    #[rhai_fn(name = "set_tag", set = "tag", return_raw)]
    pub fn set_tag(value: &mut Dynamic, tag: INT) -> RhaiResultOf<()> {
        if tag < Tag::MIN as INT {
            Err(ERR::ErrorArithmetic(
                format!(
                    "{} is too small to fit into a tag (must be between {} and {})",
                    tag,
                    Tag::MIN,
                    Tag::MAX
                ),
                Position::NONE,
            )
            .into())
        } else if tag > Tag::MAX as INT {
            Err(ERR::ErrorArithmetic(
                format!(
                    "{} is too large to fit into a tag (must be between {} and {})",
                    tag,
                    Tag::MIN,
                    Tag::MAX
                ),
                Position::NONE,
            )
            .into())
        } else {
            value.set_tag(tag as Tag);
            Ok(())
        }
    }
}
