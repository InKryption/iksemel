const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;
const StrBuffer = iksemel.StrBuffer;
const parse_helper = @import("parse_helper.zig");

pub fn ParseCtx(comptime Impl: type) type {
    return struct {
        inner: Impl,
        const Self = @This();

        /// Opens an element, providing the name. It may be followed
        /// by a series of consecutive calls to `feedElementAttribute`.
        pub fn feedElementOpen(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(name));
            return ctx.inner.feedElementOpen(name);
        }

        pub fn feedAttributeName(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(name));
            return ctx.inner.feedAttributeName(name);
        }

        pub fn feedAttributeValueSegment(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            text: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(text));
            return ctx.inner.feedAttributeValueSegment(text);
        }

        /// If `inline_close` is true, the element open tag was terminated with '/>',
        /// meaning it should not be matched against an element close tag.
        pub fn feedElementOpenEnd(
            ctx: Self,
            inline_close: bool,
        ) !void {
            return ctx.inner.feedElementOpenEnd(inline_close);
        }

        pub fn feedAttributeValueReference(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(name));
            return ctx.inner.feedAttributeValueReference(name);
        }

        pub fn feedAttributeValueEnd(ctx: Self) !void {
            return ctx.inner.feedAttributeValueEnd();
        }

        pub fn feedElementClose(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(name));
            return ctx.inner.feedElementClose(name);
        }

        pub fn feedTextDataSegment(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            text_segment: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(text_segment));
            return ctx.inner.feedTextDataSegment(text_segment);
        }

        pub fn feedTextDataReference(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime parse_helper.checkSrcType(.not_optional, @TypeOf(name));
            return ctx.inner.feedTextDataReference(name);
        }

        pub fn feedTextDataEnd(ctx: Self) !void {
            return ctx.inner.feedTextDataEnd();
        }

        pub fn feedPI(ctx: Self, data: anytype) !void {
            return ctx.inner.feedPI(data);
        }
    };
}

pub const ParseError = error{
    UnclosedElementTag,
    UnclosedElement,
    UnexpectedToken,
    UnopenedRootElement,
    AngleBracketLeftInAttributeValue,
    InvalidReferenceEnd,
    EmptyReference,
    UnclosedPI,
    EmptyPI,
    CommentDashDash,
    CommentEndTripleDash,
    InvalidCommentStartSingleDash,
    UnclosedCDataSection,
    InvalidCDataStart,
    InvalidCDataEnd,
    InvalidAngleBracketLeftBang,
};

pub inline fn parseSlice(
    /// Must be a non-streaming tokenizer.
    tokenizer: *Tokenizer,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !void {
    return parseImpl(parse_ctx_impl, tokenizer, null, .{
        .reader = {},
        .read_buffer = {},
    });
}

pub inline fn parseReader(
    /// `std.io.Reader(...)`
    reader: anytype,
    /// Used as a temporary buffer for certain reads.
    read_buffer: []u8,
    /// Used as a buffer for strings that will be forwarded to `parse_ctx_impl`.
    src_buffer: StrBuffer,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !void {
    var tokenizer = Tokenizer.initStreaming();
    return parseImpl(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buffer,
        },
        .src_buffer = src_buffer,
    });
}

fn parseImpl(
    parse_ctx_impl: anytype,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    _ = parse_ctx_impl;
    _ = tokenizer;
    _ = mbr;
}

const LabResult = enum {
    element_open,
    element_open_inline_close,
    element_close,
};
fn handleLeftAngleBracket(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    /// May be cleared at some point during this function.
    mbr_and_src: parse_helper.MBRAndSrcBuf(MaybeReader),
) !LabResult {
    const mbr = mbr_and_src.mbr;
    switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedElement,
        .equals,
        .quote_single,
        .quote_double,
        .angle_bracket_right,
        .tag_whitespace,
        => return ParseError.UnexpectedToken,
        .slash => {
            switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedElementTag,
                .equals,
                .quote_single,
                .quote_double,
                .angle_bracket_right,
                .tag_whitespace,
                .slash,
                => return ParseError.UnexpectedToken,
                .tag_token => {},
                else => unreachable,
            }

            mbr_and_src.clearSrcBuffer();
            const name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .element_tag, MaybeReader, mbr_and_src);
            try parse_ctx.feedElementClose(name);

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedElementTag,
                .tag_whitespace => unreachable,
                .slash,
                .equals,
                .quote_single,
                .quote_double,
                .tag_token,
                => return ParseError.UnexpectedToken,
                .angle_bracket_right => {},
                else => unreachable,
            }

            return .element_close;
        },
        .tag_token => {
            mbr_and_src.clearSrcBuffer();
            try parse_ctx.feedElementOpen(try parse_helper.nextTokenFullStrOrRange(tokenizer, .element_tag, MaybeReader, mbr_and_src));

            const inline_close = while (true) {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    .eof => return ParseError.UnclosedElementTag,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => break false,
                    .slash => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedToken,
                        .eof => return ParseError.UnclosedElementTag,
                        .angle_bracket_right => break true,
                    },

                    .equals,
                    .quote_single,
                    .quote_double,
                    => return ParseError.UnexpectedToken,

                    .tag_token => {},

                    else => unreachable,
                }
                mbr_and_src.clearSrcBuffer();
                try parse_ctx.feedAttributeName(try parse_helper.nextTokenFullStrOrRange(tokenizer, .element_tag, MaybeReader, mbr_and_src));

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedToken,
                    .eof => return ParseError.UnclosedElementTag,
                    .equals => {},
                }

                const open_quote = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedToken,
                    .eof => return ParseError.UnclosedElementTag,
                    .quote_single,
                    .quote_double,
                    => |open_quote| open_quote,
                };
                const str_ctx: Tokenizer.Context = switch (open_quote) {
                    .quote_single => .attribute_value_quote_single,
                    .quote_double => .attribute_value_quote_double,
                    else => unreachable,
                };

                while (true) switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                    .angle_bracket_left => return ParseError.AngleBracketLeftInAttributeValue,

                    .quote_single,
                    .quote_double,
                    => |close_quote| {
                        assert(open_quote == close_quote);
                        break;
                    },

                    .text_data => {
                        if (MaybeReader != null) {
                            while (try parse_helper.nextTokenSegment(tokenizer, str_ctx, mbr.reader, mbr.read_buffer)) |segment| {
                                try parse_ctx.feedAttributeValueSegment(segment);
                            }
                        } else {
                            const range = tokenizer.nextSrcNoUnderrun(str_ctx);
                            try parse_ctx.feedAttributeValueSegment(range);
                        }
                    },

                    .ampersand => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                        .semicolon => return ParseError.EmptyReference,
                        .tag_token => {
                            mbr_and_src.clearSrcBuffer();
                            const ref_name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .reference, MaybeReader, mbr_and_src);
                            try parse_ctx.feedAttributeValueReference(ref_name);
                        },
                    },

                    else => unreachable,
                };
                try parse_ctx.feedAttributeValueEnd();
            };

            try parse_ctx.feedElementOpenEnd(inline_close);
            return if (inline_close)
                .element_open_inline_close
            else
                .element_open;
        },
        else => unreachable,
    }
}

pub const IgnoreCtx = struct {
    pub fn feedElementOpen(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedAttributeName(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedAttributeValueSegment(
        ctx: @This(),
        text: anytype,
    ) !void {
        _ = ctx;
        _ = text;
    }

    pub fn feedElementOpenEnd(
        ctx: @This(),
        inline_close: bool,
    ) !void {
        _ = ctx;
        _ = inline_close;
    }

    pub fn feedAttributeValueReference(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedAttributeValueEnd(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedElementClose(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedTextDataSegment(
        ctx: @This(),
        text_segment: anytype,
    ) !void {
        _ = ctx;
        _ = text_segment;
    }

    pub fn feedTextDataReference(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedTextDataEnd(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedPI(ctx: @This(), data: anytype) !void {
        _ = ctx;
        _ = data;
    }
};

test parseSlice {
    var tokenizer = Tokenizer.initComplete("<foo/>\n");
    assert(iksemel.prolog.parseSlice(&tokenizer, iksemel.prolog.IgnoreCtx{}) catch unreachable == .angle_bracket_left);
    try std.testing.expectEqual({}, parseSlice(&tokenizer, IgnoreCtx{}));
}
