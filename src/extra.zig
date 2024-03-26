pub fn containsOnlyValidPubidLiteralCharacters(
    str: []const u8,
    /// The surrounding quote type.
    ///
    /// A pubid literal may never contain a double quote,
    /// however it may contain a single quote if the string
    /// is double quoted.
    quote: Tokenizer.QuoteType,
) bool {
    return for (str) |char| switch (char) {
        '\u{20}', '\u{D}', '\u{A}' => {},
        'a'...'z', 'A'...'Z', '0'...'9' => {},
        '-' => {},
        '\'' => switch (quote) {
            .double => {},
            .single => break false,
        },
        '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' => {},
        else => break false,
    } else true;
}

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;
