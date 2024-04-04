//! Utilities for scanning the DTD Internal Subset.
//! Not suitable for rigorous, correct scanning of any
//! DTD External Subset, as parameter entity references
//! are disallowed inside markup declarations in the
//! Internal Subset, but not in the External Subset.

const markup_decl_kind = @import("dtd/markup_decl_kind.zig");
pub const MarkupDeclKind = markup_decl_kind.MarkupDeclKind;

pub const entity = @import("dtd/entity.zig");
pub const element = @import("dtd/element.zig");
pub const attlist = @import("dtd/attlist.zig");
pub const notation = @import("dtd/notation.zig");

comptime {
    _ = entity;
    _ = element;
    _ = attlist;
    _ = notation;
    _ = markup_decl_kind;
}
