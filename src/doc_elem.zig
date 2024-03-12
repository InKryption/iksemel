const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("parse_helper.zig");

pub const ElementOpenEnd = enum {
    normal,
    inline_close,
};

pub fn ParseCtx(comptime Impl: type) type {
    return struct {
        inner: Impl,
        const Self = @This();

        /// Called consecutively to construct the source.
        /// Terminated when `segment == null`.
        pub fn feedSrc(
            ctx: Self,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            segment: anytype,
        ) !void {
            switch (@TypeOf(segment)) {
                ?[]const u8, ?Tokenizer.Range => {},
                else => @compileError(
                    "" ++
                        "Expected one of " ++
                        @typeName(?[]const u8) ++
                        " or " ++
                        @typeName(?Tokenizer.Range) ++
                        ", instead got " ++
                        @typeName(@TypeOf(segment)),
                ),
            }
            return ctx.inner.feedSrc(segment);
        }

        pub fn feedMarker(ctx: Self, marker: ParseMarker) !void {
            return ctx.inner.feedMarker(marker);
        }
    };
}

pub const ParseMarker = union(enum) {
    /// Followed by `feedSrc*` to construct the reference id/name.
    reference,
    /// Followed by `feedSrc*` to construct the text data.
    text,

    /// Followed by `feedSrc*` to consruct the name.
    /// Afterwards, it may be followed by one of:
    /// * `attribute_start`
    /// * `element_open_end`
    element_open_start,

    /// May be followed by one of:
    /// * `element_open_start`
    /// * `text`
    /// * `reference`
    /// * `element_close`
    element_open_end: ElementOpenEnd,

    /// Followed by `feedSrc*` to construct the name.
    element_close,

    /// Followed by `feedSrc*` to construct the attribute name.
    /// Afterwards, it will be followed by a series of
    /// interspersed `reference`s and `text`s, terminated
    /// by `attribute_end`.
    attribute_start,
    /// Terminates the attribute.
    /// May be followed by one of:
    /// * `element_open_end`
    attribute_end,

    /// Followed by `feedSrc*` to construct the Processing Instructions.
    /// Interpretation and validation of the PI are left up to the context.
    /// NOTE: the procedure does not validate the target, or that there
    /// even is a target.
    pi,
};

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
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !void {
    var tokenizer = Tokenizer.initStreaming();
    return parseImpl(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .reader = reader,
        .read_buffer = read_buffer,
    });
}

fn parseImpl(
    parse_ctx_impl: anytype,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const Impl = @TypeOf(parse_ctx_impl);
    const parse_ctx: ParseCtx(Impl) = .{ .inner = parse_ctx_impl };

    const root_has_children = switch (try handleLeftAngleBracket(Impl, parse_ctx, tokenizer, MaybeReader, mbr)) {
        .element_open => true,
        .element_open_inline_close => false,
        .element_close => return ParseError.UnopenedRootElement,
    };

    var depth: u64 = @intFromBool(root_has_children);

    if (root_has_children) while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedElement,
        .angle_bracket_left => switch (try handleLeftAngleBracket(Impl, parse_ctx, tokenizer, MaybeReader, mbr)) {
            .element_open => {
                depth += 1;
            },
            .element_open_inline_close => {},
            .element_close => {
                depth -= 1;
                if (depth == 0) break;
            },
        },
        .text_data => {
            try parse_ctx.feedMarker(.text);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .non_markup, MaybeReader, mbr);
        },
        .ampersand => try handleAmpersand(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

        .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
            .normal_end => {},
            .invalid_end_triple_dash => return ParseError.CommentEndTripleDash,
            .invalid_dash_dash => return ParseError.CommentDashDash,
        },
        .cdata_start => switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
            else => unreachable,
            .eof => return ParseError.UnclosedCDataSection,
            .cdata_end => {},
            .text_data => {
                try parse_ctx.feedMarker(.text);
                try feedTokenSrc(Impl, parse_ctx, tokenizer, .cdata, MaybeReader, mbr);

                switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return ParseError.UnclosedCDataSection,
                    .text_data => unreachable,
                    .cdata_end => {},
                }
            },
        },
        .pi_start => switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
            else => unreachable,
            .eof => return ParseError.UnclosedPI,
            .pi_end => return ParseError.EmptyPI,
            .text_data => {
                try parse_ctx.feedMarker(.pi);
                try feedTokenSrc(Impl, parse_ctx, tokenizer, .pi, MaybeReader, mbr);

                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return ParseError.UnclosedPI,
                    .pi_end => {},
                }
            },
        },
        .invalid_comment_start_single_dash => return ParseError.InvalidCommentStartSingleDash,
        .invalid_cdata_start => return ParseError.InvalidCDataStart,
        .cdata_end => return ParseError.InvalidCDataEnd,
        .dtd_start => return ParseError.UnexpectedToken,
        .invalid_dtd_start => return ParseError.UnexpectedToken,
        .invalid_angle_bracket_left_bang => return ParseError.UnexpectedToken,
        else => unreachable,
    };

    while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof => break,
        .text_data => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .non_markup, MaybeReader, mbr)) {
            .all_whitespace => {},
            .non_whitespace => return ParseError.UnexpectedToken,
        },
        else => return ParseError.UnexpectedToken,
    };
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
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !LabResult {
    switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedElementTag,
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

            try parse_ctx.feedMarker(.element_close);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

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
            try parse_ctx.feedMarker(.element_open_start);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

            const end_kind: ElementOpenEnd = while (true) {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    .eof => return ParseError.UnclosedElementTag,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => break .normal,
                    .slash => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedToken,
                        .eof => return ParseError.UnclosedElementTag,
                        .angle_bracket_right => break .inline_close,
                    },

                    .equals,
                    .quote_single,
                    .quote_double,
                    => return ParseError.UnexpectedToken,

                    .tag_token => {},

                    else => unreachable,
                }

                try parse_ctx.feedMarker(.attribute_start);
                try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

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
                        try parse_ctx.feedMarker(.text);
                        try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr);
                    },

                    .ampersand => try handleAmpersand(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

                    else => unreachable,
                };
                try parse_ctx.feedMarker(.attribute_end);
            };

            try parse_ctx.feedMarker(.{ .element_open_end = end_kind });
            return switch (end_kind) {
                .normal => .element_open,
                .inline_close => .element_open_inline_close,
            };
        },
        else => unreachable,
    }
}

fn handleAmpersand(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    /// May be cleared at some point during this function.
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        else => unreachable,
        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
        .semicolon => return ParseError.EmptyReference,
        .tag_token => {},
    }
    try parse_ctx.feedMarker(.reference);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .reference, MaybeReader, mbr);
    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        else => unreachable,
        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
        .semicolon => {},
        .tag_token => unreachable,
    }
}

fn feedTokenSrc(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
    while (true) {
        const segment = try iter.next(tokenizer, context, mbr);
        try parse_ctx.feedSrc(segment);
        if (segment == null) break;
    }
}

pub const IgnoreCtx = struct {
    pub fn feedSrc(
        ctx: @This(),
        segment: anytype,
    ) !void {
        _ = segment;
        _ = ctx;
    }

    pub fn feedMarker(ctx: @This(), marker: ParseMarker) !void {
        _ = marker;
        _ = ctx;
    }
};

test parseSlice {
    var tokenizer = Tokenizer.initComplete("<foo/>\n");
    assert(iksemel.prolog.parseSlice(&tokenizer, iksemel.prolog.IgnoreCtx{}) catch unreachable == .angle_bracket_left);
    try std.testing.expectEqual({}, parseSlice(&tokenizer, IgnoreCtx{}));
}
