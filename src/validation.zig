//! Tools for interpreting strings and individual characters
//! in strings according to certain XML constructs.

/// The set of codepoints defined as whitespace. They are all
/// exactly one byte in size.
pub const whitespace_set: []const u8 = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
};

pub fn containsOnlyValidPubidLiteralCharacters(
    str: []const u8,
    /// The surrounding quote type.
    ///
    /// A pubid literal may never contain a double quote,
    /// however it may contain a single quote if the string
    /// is double quoted.
    quote: Tokenizer.QuoteType,
) bool {
    return for (str) |char| {
        if (!isPubidChar(char, quote)) break false;
    } else true;
}

pub fn isPubidChar(
    codepoint: u8,
    /// The surrounding quote type.
    ///
    /// A pubid literal may never contain a double quote,
    /// however it may contain a single quote if the string
    /// is double quoted.
    quote: Tokenizer.QuoteType,
) bool {
    return switch (codepoint) {
        '\u{20}', '\u{D}', '\u{A}' => true,
        'a'...'z', 'A'...'Z', '0'...'9' => true,
        '-' => true,
        '\'' => switch (quote) {
            .single => false,
            .double => true,
        },
        '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' => true,
        else => false,
    };
}

/// Returns true for any valid name character.
/// Any character for whic this is true is at least
/// valid anywhere after the first character, and
/// *may* be valid as the first character.
pub inline fn isNameChar(codepoint: u21) bool {
    return isNameStartChar(codepoint) or isNameCharAfterStart(codepoint);
}

/// Returns true for characters which are valid as the
/// start of a name. Any character for which this is true
/// is also a valid character anywhere else in the name.
pub fn isNameStartChar(codepoint: u21) bool {
    return switch (codepoint) {
        ':' => true,
        'A'...'Z' => true,
        '_' => true,
        'a'...'z' => true,
        '\u{C0}'...'\u{D6}' => true,
        '\u{D8}'...'\u{F6}' => true,
        '\u{F8}'...'\u{2FF}' => true,
        '\u{370}'...'\u{37D}' => true,
        '\u{37F}'...'\u{1FFF}' => true,
        '\u{200C}'...'\u{200D}' => true,
        '\u{2070}'...'\u{218F}' => true,
        '\u{2C00}'...'\u{2FEF}' => true,
        '\u{3001}'...'\u{D7FF}' => true,
        '\u{F900}'...'\u{FDCF}' => true,
        '\u{FDF0}'...'\u{FFFD}' => true,
        '\u{10000}'...'\u{EFFFF}' => true,
        else => false,
    };
}

/// Returns true for characters which are valid as
/// part of a name, and cannot be the start of a name.
pub fn isNameCharAfterStart(codepoint: u21) bool {
    return switch (codepoint) {
        '-' => true,
        '.' => true,
        '0'...'9' => true,
        '\u{B7}' => true,
        '\u{0300}'...'\u{036F}' => true,
        '\u{203F}'...'\u{2040}' => true,
        else => false,
    };
}

pub const legal_char = struct {
    /// The maximum value representing a unicode codepoint recognized
    /// as a legal production of a character in an XML document.
    pub const max_value = '\u{10FFFF}';
    /// The maximum length of 'digits' in `'&#' Digits ';'`
    pub const max_ref_digits_dec = std.fmt.count("{d}", .{max_value});
    /// The maximum length of 'digits' in `'&#x' Digits ';'`
    pub const max_ref_digits_hex = std.fmt.count("{x}", .{max_value});
};

/// Returns true if `codepoint` matches the production for Char as defined by the XML 1.0 specification.
pub inline fn isLegalChar(codepoint: u21) bool {
    return switch (codepoint) {
        '\u{9}' => true,
        '\u{A}' => true,
        '\u{D}' => true,
        '\u{20}'...'\u{D7FF}' => true,
        '\u{E000}'...'\u{FFFD}' => true,
        '\u{10000}'...'\u{10FFFF}' => true,
        else => false,
    };
}

pub const ParseCharRefError = error{
    /// The reference is empty ('&;').
    EmptyReference,
    /// The reference is missing its hexadecimal digits ('&x;').
    MissingDigits,
    /// The reference contains invalid digits (either digits invalid for its base, or non-digit characters).
    InvalidDigits,
    /// The reference parses to an invalid codepoint.
    InvalidCodepoint,
};
/// The returned codepoint is not validated to be a legal character;
/// that can be checked with `isLegalChar` afterwards.
pub fn parseCharRef(
    /// From: `'&#' ref ';'`
    ref: []const u8,
) ParseCharRefError!u21 {
    if (ref.len == 0) return ParseCharRefError.EmptyReference;
    const digits: []const u8, const base: u8 = switch (ref[0]) {
        'x' => .{ ref[1..], 16 },
        else => .{ ref, 10 },
    };
    if (ref.len == 0) return ParseCharRefError.MissingDigits;

    const max_ref_digits: u16 = switch (base) {
        10 => legal_char.max_ref_digits_dec,
        16 => legal_char.max_ref_digits_hex,
        else => unreachable,
    };
    if (digits.len >= max_ref_digits) {}

    if (std.mem.indexOfScalar(u8, digits, '_') != null) {
        return ParseCharRefError.InvalidDigits;
    }

    return std.fmt.parseUnsigned(u21, digits, base) catch |err| switch (err) {
        error.Overflow => ParseCharRefError.InvalidCodepoint,
        error.InvalidCharacter => ParseCharRefError.InvalidDigits,
    };
}

const std = @import("std");

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;
