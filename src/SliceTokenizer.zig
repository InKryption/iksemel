//! A thin wrapper over `Tokenizer`, which is initialised with
//! the entirety of the XML source, expressing the impossibility
//! of encountering `error.BufferUnderrun` in the API.

const std = @import("std");
const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const SliceTokenizer = @This();
inner: Tokenizer,

pub inline fn init(src: []const u8) SliceTokenizer {
    return .{ .inner = Tokenizer.initComplete(src) };
}

pub inline fn nextType(st: *SliceTokenizer) Tokenizer.TokenType {
    return st.inner.nextType() catch |err| switch (err) {
        error.BufferUnderrun => unreachable,
    };
}

pub inline fn nextSrc(st: *SliceTokenizer) ?Tokenizer.TokenSrc {
    return st.inner.nextSrc() catch |err| switch (err) {
        error.BufferUnderrun => unreachable,
    };
}

pub inline fn nextString(st: *SliceTokenizer) ?[]const u8 {
    return st.inner.nextString() catch |err| switch (err) {
        error.BufferUnderrun => unreachable,
    };
}
