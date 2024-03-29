//! Tools for validating strings and individual characters
//! in strings according to certain XML constructs.

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

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;
