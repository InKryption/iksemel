const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");

pub const CommentSegment = union(enum) {
    text: iksemel.Tokenizer.TokenSrc,
    /// The '--' token.
    invalid_double_dash,
    /// The '--->' token.
    invalid_triple_dash_end,
    /// The '-->' token.
    end,
};

pub const AttributeSegment = union(enum) {
    text: iksemel.Tokenizer.TokenSrc,
    /// Invalid token '<' in the attribute value.
    invalid_angle_bracket_left,
    /// Ends the sequence
    end_quote,
};

pub const InvalidToken = enum {
    slash,
    angle_bracket_left_bang,

    pub inline fn toStr(it: InvalidToken) []const u8 {
        return switch (it) {
            .slash => "/",
            .angle_bracket_left_bang => "<!",
        };
    }
};

pub const QuoteType = enum {
    single,
    double,
};

pub inline fn parseReadingTokenizer(
    /// Must conform to the interface defined by `ParseContext`.
    parse_ctx: anytype,
    /// `*iksemel.ReadingTokenizer(Src)`
    reading_tokenizer: anytype,
) (ParseContextGeneralErrorSet(@TypeOf(parse_ctx)) || @TypeOf(reading_tokenizer.*).Error)!void {
    return parseReaderOrSlice(parse_ctx, @TypeOf(reading_tokenizer.*).Src, reading_tokenizer);
}

pub inline fn parseSliceTokenizer(
    /// Must conform to the interface defined by `ParseContext`.
    parse_ctx: anytype,
    /// Must be non-streaming (containing the entirety of the source),
    /// as the `*AssumeNoUnderrun` will be used.
    slice_tokenizer: *iksemel.Tokenizer,
) ParseContextGeneralErrorSet(@TypeOf(parse_ctx))!void {
    return parseReaderOrSlice(parse_ctx, null, slice_tokenizer);
}

/// Exists simply to define and verify the expected interface of the context used for parsing.
pub fn ParseContext(comptime InnerCtx: type) type {
    return struct {
        inner: InnerCtx,
        const Self = @This();

        const Ctx = switch (@typeInfo(InnerCtx)) {
            .Pointer => |pointer| pointer.child,
            else => InnerCtx,
        };

        /// Signals that eof has been reached. No more tokenization will occur.
        pub fn feedEof(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedEof, .{ctx.inner});
        }
        /// Provides an invalid token in with meaning based on the surrounding context.
        pub fn feedInvalidToken(ctx: Self, invalid_token: InvalidToken) !void {
            return @call(.always_inline, Ctx.feedInvalidToken, .{ ctx.inner, invalid_token });
        }
        /// Indicates the start of a run of text data. Followed by calls to `feedTextData`.
        pub fn feedTextDataStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedTextDataStart, .{ctx.inner});
        }
        /// The provided `text_src` represents text data with meaning based on the surrounding context.
        /// When `text_src` is null, the sequence has terminated.
        pub fn feedTextDataSegment(ctx: Self, text_src: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedTextDataSegment, .{ ctx.inner, text_src });
        }
        /// Indicates that a CDATA section has been opened. If `invalid != null`, the '<![CDATA[' token is
        /// partial, and the `invalid` parameter represents said partial token.
        pub fn feedCdataStart(ctx: Self, invalid: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedCdataStart, .{ ctx.inner, invalid });
        }
        /// Indicates that the currently open CDATA section has been closed by a ']]>' token.
        /// May also appear alone as an invalid token.
        pub fn feedCdataEnd(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedCdataEnd, .{ctx.inner});
        }
        /// Indicates that a comment has been opened. Followed by a series of calls to `feedCommentSegment`.
        pub fn feedCommentStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedCommentStart, .{ctx.inner});
        }
        /// Provides a segment of the current comment. Terminated when `segment == .end`,
        /// or `segment == .invalid_triple_dash_end`.
        pub fn feedCommentSegment(ctx: Self, segment: CommentSegment) !void {
            return @call(.always_inline, Ctx.feedCommentSegment, .{ ctx.inner, segment });
        }
        /// Indicates that a PI directive has been opened.
        pub fn feedPiStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedPiStart, .{ctx.inner});
        }
        /// Provides a segment of the PI directive. Terminates when `data_src == null`.
        pub fn feedPiData(ctx: Self, data_src: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedPiData, .{ ctx.inner, data_src });
        }
        /// Indicates that the currently open PI directive has been closed by a '?>'.
        pub fn feedPiEnd(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedPiEnd, .{ctx.inner});
        }
        /// Indicates that a reference has been started with '&'.
        pub fn feedReferenceStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedReferenceStart, .{ctx.inner});
        }
        /// Provides a segment of the current reference. Terminated when `tag_src == null`.
        pub fn feedReferenceSegment(ctx: Self, tag_src: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedReferenceSegment, .{ ctx.inner, tag_src });
        }
        /// Indicates that the current reference was ended either with a valid (;) or invalid terminating character.
        pub fn feedReferenceEnd(ctx: Self, valid: bool) !void {
            return @call(.always_inline, Ctx.feedReferenceEnd, .{ ctx.inner, valid });
        }
        /// Indicates that an element open tag has been started with '<'.
        pub fn feedElementOpenStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedElementOpenStart, .{ctx.inner});
        }
        /// Indicates that an element close tag has been started with '</'.
        pub fn feedElementCloseStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedElementCloseStart, .{ctx.inner});
        }
        /// Provides a segment of the recently opened element's name. Terminates when `name_segment == null`.
        /// Used after both `feedElementOpenStart` and `feedElementCloseStart`.
        pub fn feedElementTagNameSegment(ctx: Self, name: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedElementTagNameSegment, .{ ctx.inner, name });
        }
        /// Indicates that the current element tag has been ended with '>'.
        /// Used after `feedElementOpenStart` and `feedElementCloseStart`.
        pub fn feedElementTagEnd(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedElementTagEnd, .{ctx.inner});
        }
        /// Indicates that the current element open tag has been ended with '/>'.
        /// Used after `feedElementOpenStart`.
        pub fn feedElementOpenEndInlineClose(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedElementOpenEndInlineClose, .{ctx.inner});
        }
        /// Indicates the start of an attribute name.
        pub fn feedAttributeNameStart(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedAttributeNameStart, .{ctx.inner});
        }
        /// Provides a segment of an attribute name. Terminates when `name_segment == null`
        pub fn feedAttributeNameSegment(ctx: Self, name_segment: ?iksemel.Tokenizer.TokenSrc) !void {
            return @call(.always_inline, Ctx.feedAttributeNameSegment, .{ ctx.inner, name_segment });
        }
        /// Indicates the equals token '='.
        pub fn feedAttributeEquals(ctx: Self) !void {
            return @call(.always_inline, Ctx.feedAttributeEquals, .{ctx.inner});
        }
        /// Indicates the start of an attribute value.
        pub fn feedAttributeValueStart(ctx: Self, quote_type: QuoteType) !void {
            return @call(.always_inline, Ctx.feedAttributeValueStart, .{ ctx.inner, quote_type });
        }
        /// Provides a segment of an attribute value. Terminates when `value_segment == .end_quote`.
        pub fn feedAttributeValueSegment(ctx: Self, value_segment: AttributeSegment) !void {
            return @call(.always_inline, Ctx.feedAttributeValueSegment, .{ ctx.inner, value_segment });
        }
    };
}

pub fn ParseContextGeneralErrorSet(comptime Ctx: type) type {
    const Pc = ParseContext(Ctx);
    var GeneralErrSet = error{};
    const decls = @typeInfo(Pc).Struct.decls;
    @setEvalBranchQuota(decls.len + 1);
    for (decls) |decl| {
        const DeclType = @TypeOf(@field(Pc, decl.name));
        if (@typeInfo(DeclType) != .Fn) continue;
        const decl_info = @typeInfo(DeclType).Fn;
        const RetType = decl_info.return_type.?;
        const ErrSet = switch (@typeInfo(RetType)) {
            .ErrorSet => RetType,
            .ErrorUnion => |err_un| err_un.error_set,
            else => error{},
        };
        GeneralErrSet = GeneralErrSet || ErrSet;
    }
    return GeneralErrSet;
}

fn parseReaderOrSlice(
    parse_ctx: anytype,
    comptime MaybeReader: ?type,
    tokenizer: *if (MaybeReader) |SrcReader| iksemel.ReadingTokenizer(SrcReader) else iksemel.Tokenizer,
) !void {
    const ctx: ParseContext(@TypeOf(parse_ctx)) = .{ .inner = parse_ctx };

    const helpers = struct {
        inline fn feedReferenceAfterHavingEncounteredAmpersand(
            _ctx: @TypeOf(ctx),
            _tokenizer: @TypeOf(tokenizer),
        ) !void {
            try _ctx.feedReferenceStart();
            switch (try getNextTokType(_tokenizer)) {
                .tag_token => {
                    while (true) {
                        const maybe_tag_token = try getNextTokSrc(_tokenizer);
                        try _ctx.feedReferenceSegment(maybe_tag_token);
                        if (maybe_tag_token == null) break;
                    }
                    try _ctx.feedReferenceEnd(switch (try getNextTokType(_tokenizer)) {
                        .semicolon => true,
                        .invalid_reference_end => false,
                        else => unreachable,
                    });
                },
                .semicolon => try _ctx.feedReferenceEnd(true),
                .invalid_reference_end => try _ctx.feedReferenceEnd(false),
                else => unreachable,
            }
        }
    };

    mainloop: while (true) {
        switch (try getNextTokType(tokenizer)) {
            .eof => break :mainloop try ctx.feedEof(),

            .dtd_start => @panic("TODO"),
            .invalid_dtd_start => @panic("TODO"),

            .text_data => {
                try ctx.feedTextDataStart();
                while (true) {
                    const maybe_text_data = try getNextTokSrc(tokenizer);
                    try ctx.feedTextDataSegment(maybe_text_data);
                    if (maybe_text_data == null) break;
                }
            },
            .cdata_end => try ctx.feedCdataEnd(),

            .angle_bracket_left => {
                const is_close_tag: bool, //
                const token_type_after_name: ?iksemel.Tokenizer.TokenType //
                = blk: {
                    var is_close_tag = false;
                    const token_type_after_name = while (true) break switch (try getNextTokType(tokenizer)) {
                        .slash => |tag| {
                            if (is_close_tag) break tag;
                            is_close_tag = true;
                            try ctx.feedElementCloseStart();
                            continue;
                        },
                        .tag_token => {
                            if (!is_close_tag) try ctx.feedElementOpenStart();
                            while (true) {
                                const maybe_tag_token = try getNextTokSrc(tokenizer);
                                try ctx.feedElementTagNameSegment(maybe_tag_token);
                                if (maybe_tag_token == null) break;
                            }
                            break null;
                        },

                        .tag_whitespace,
                        .equals,
                        .quote_single,
                        .quote_double,
                        => |tag| {
                            if (!is_close_tag) try ctx.feedElementOpenStart();
                            break tag;
                        },
                        .angle_bracket_right => {
                            if (!is_close_tag) try ctx.feedElementOpenStart();
                            try ctx.feedElementTagEnd();
                            continue :mainloop;
                        },
                        .eof => {
                            if (!is_close_tag) try ctx.feedElementOpenStart();
                            try ctx.feedEof();
                            break :mainloop;
                        },
                        else => unreachable,
                    };
                    break :blk .{
                        is_close_tag,
                        token_type_after_name,
                    };
                };

                var queued_token_type: ?iksemel.Tokenizer.TokenType = token_type_after_name;
                while (true) switch (blk: {
                    defer queued_token_type = null;
                    break :blk queued_token_type orelse try getNextTokType(tokenizer);
                }) {
                    .tag_token => {
                        try ctx.feedAttributeNameStart();
                        while (true) {
                            const maybe_tag_token = try getNextTokSrc(tokenizer);
                            try ctx.feedAttributeNameSegment(maybe_tag_token);
                            if (maybe_tag_token == null) break;
                        }
                    },
                    .tag_whitespace => while (try getNextTokSrc(tokenizer)) |_| {},
                    .equals => try ctx.feedAttributeEquals(),
                    .quote_single,
                    .quote_double,
                    => |open_quote_type| {
                        try ctx.feedAttributeValueStart(switch (open_quote_type) {
                            .quote_single => .single,
                            .quote_double => .double,
                            else => unreachable,
                        });
                        while (true) switch (try getNextTokType(tokenizer)) {
                            .text_data => while (try getNextTokSrc(tokenizer)) |text_data| {
                                try ctx.feedAttributeValueSegment(.{ .text = text_data });
                            },
                            .ampersand => try helpers.feedReferenceAfterHavingEncounteredAmpersand(ctx, tokenizer),
                            .angle_bracket_left => try ctx.feedAttributeValueSegment(.invalid_angle_bracket_left),
                            .quote_single,
                            .quote_double,
                            => |close_quote_type| {
                                assert(open_quote_type == close_quote_type);
                                try ctx.feedAttributeValueSegment(.end_quote);
                                break;
                            },
                            .eof => break :mainloop try ctx.feedEof(),
                            else => unreachable,
                        };
                    },
                    .slash => {
                        if (is_close_tag) {
                            try ctx.feedInvalidToken(.slash);
                        } else switch (try getNextTokType(tokenizer)) {
                            else => |tt_to_queue| queued_token_type = tt_to_queue,
                            .angle_bracket_right => {
                                try ctx.feedElementOpenEndInlineClose();
                                continue :mainloop;
                            },
                        }
                    },
                    .angle_bracket_right => {
                        try ctx.feedElementTagEnd();
                        continue :mainloop;
                    },
                    .eof => break :mainloop try ctx.feedEof(),
                    else => unreachable,
                };
            },

            .ampersand => try helpers.feedReferenceAfterHavingEncounteredAmpersand(ctx, tokenizer),

            .pi_start => {
                try ctx.feedPiStart();
                switch (try getNextTokType(tokenizer)) {
                    .text_data => {
                        while (true) {
                            const maybe_text_data = try getNextTokSrc(tokenizer);
                            try ctx.feedPiData(maybe_text_data);
                            if (maybe_text_data == null) break;
                        }
                        switch (try getNextTokType(tokenizer)) {
                            .pi_end => try ctx.feedPiEnd(),
                            .eof => break :mainloop try ctx.feedEof(),
                            else => unreachable,
                        }
                    },
                    .pi_end => try ctx.feedPiEnd(),
                    .eof => break :mainloop try ctx.feedEof(),
                    else => unreachable,
                }
            },
            .invalid_angle_bracket_left_bang => try ctx.feedInvalidToken(.angle_bracket_left_bang),

            .cdata_start,
            .invalid_cdata_start,
            => |tag| {
                try ctx.feedCdataStart(switch (tag) {
                    .cdata_start => null,
                    .invalid_cdata_start => blk: {
                        const invalid_src = (try getNextTokSrc(tokenizer)).?;
                        assert((try getNextTokSrc(tokenizer)) == null);
                        break :blk invalid_src;
                    },
                    else => unreachable,
                });
                switch (try getNextTokType(tokenizer)) {
                    .text_data => {
                        while (true) {
                            const maybe_text_data = try getNextTokSrc(tokenizer);
                            try ctx.feedTextDataSegment(maybe_text_data);
                            if (maybe_text_data == null) break;
                        }
                        switch (try getNextTokType(tokenizer)) {
                            .cdata_end => try ctx.feedCdataEnd(),
                            .eof => break :mainloop try ctx.feedEof(),
                            else => unreachable,
                        }
                    },
                    .cdata_end => try ctx.feedCdataEnd(),
                    .eof => break :mainloop try ctx.feedEof(),
                    else => unreachable,
                }
            },

            .comment_start => {
                try ctx.feedCommentStart();
                while (true) switch (try getNextTokType(tokenizer)) {
                    .text_data => while (try getNextTokSrc(tokenizer)) |tok_src| {
                        try ctx.feedCommentSegment(CommentSegment{ .text = tok_src });
                    },
                    .invalid_comment_dash_dash => try ctx.feedCommentSegment(CommentSegment{ .invalid_double_dash = {} }),
                    .invalid_comment_end_triple_dash => break try ctx.feedCommentSegment(CommentSegment{ .invalid_double_dash = {} }),
                    .comment_end => break try ctx.feedCommentSegment(CommentSegment{ .end = {} }),
                    .eof => break :mainloop try ctx.feedEof(),
                    else => unreachable,
                };
            },

            .element_decl => unreachable,
            .entity_decl => unreachable,
            .attlist_decl => unreachable,
            .notation_decl => unreachable,

            .tag_whitespace => unreachable,
            .tag_token => unreachable,
            .invalid_token => unreachable,

            .equals => unreachable,

            .lparen => unreachable,
            .rparen => unreachable,

            .pipe => unreachable,
            .comma => unreachable,
            .qmark => unreachable,
            .asterisk => unreachable,
            .plus => unreachable,

            .slash => unreachable,

            .percent => unreachable,

            .quote_single => unreachable,
            .quote_double => unreachable,

            .angle_bracket_right => unreachable,

            .square_bracket_left => unreachable,
            .square_bracket_right => unreachable,

            .semicolon => unreachable,
            .invalid_reference_end => unreachable,

            .pi_end => unreachable,

            .invalid_comment_start_single_dash => unreachable,
            .invalid_comment_dash_dash => unreachable,
            .invalid_comment_end_triple_dash => unreachable,
            .comment_end => unreachable,
        }
    }
}

inline fn getNextTokType(tokenizer: anytype) !iksemel.Tokenizer.TokenType {
    return switch (@TypeOf(tokenizer.*)) {
        iksemel.Tokenizer => tokenizer.nextTypeAssumeNoUnderrun(),
        else => tokenizer.nextType(),
    };
}
inline fn getNextTokSrc(tokenizer: anytype) !?iksemel.Tokenizer.TokenSrc {
    return switch (@TypeOf(tokenizer.*)) {
        iksemel.Tokenizer => tokenizer.nextSrcAssumeNoUnderrun(),
        else => tokenizer.nextType(),
    };
}

test {
    var tokenizer = iksemel.Tokenizer.initComplete(
        \\
        \\<foo>
        \\  <bar fizz = 'buzz'>baz</bar>
        \\</foo>
        \\
    );

    const Ctx = struct {
        tokenizer: *const iksemel.Tokenizer,
        arena: std.mem.Allocator,
        root: Node.Element = .{
            .name = "",
            .attributes = &.{},
            .children = &.{},
        },
        parent_stack: std.ArrayListUnmanaged(*Node.Element) = .{},
        current: *Node.Element,
        element_tag_state: ?enum { open, close } = null,

        const Node = union(enum) {
            text: []const u8,
            element: Element,
            element_close: ElementClose,

            const Element = struct {
                name: []const u8,
                attributes: []const Attribute,
                children: []const Node,
            };
            const Attribute = struct {
                name: []const u8,
                eql: bool,
                value: ?[]const u8,
            };
            const ElementClose = struct {
                name: []const u8,
                invalid_tokens: []const []const u8,
            };
        };

        inline fn appendNodeToChildren(ctx: @This(), value: Node) !void {
            const realloced_children: []Node = try ctx.arena.realloc(@constCast(ctx.current.children), ctx.current.children.len + 1);
            realloced_children[realloced_children.len - 1] = value;
            ctx.current.children = realloced_children;
        }
        fn latestChild(ctx: @This()) *Node {
            return @constCast(&ctx.current.children[ctx.current.children.len - 1]);
        }

        pub inline fn feedEof(ctx: *@This()) !void {
            _ = ctx;
        }
        pub inline fn feedInvalidToken(ctx: *@This(), invalid_token: InvalidToken) !void {
            _ = ctx;
            _ = invalid_token;
        }
        pub inline fn feedTextDataStart(ctx: *@This()) !void {
            try ctx.appendNodeToChildren(.{ .text = "" });
        }
        pub inline fn feedTextDataSegment(ctx: *@This(), text_src: ?iksemel.Tokenizer.TokenSrc) !void {
            const text_segment = ctx.tokenizer.getSrcString(text_src orelse return);
            const text_ptr: *[]const u8 = &ctx.latestChild().text;
            const text_realloced = try ctx.arena.realloc(@constCast(text_ptr.*), text_ptr.len + text_segment.len);
            @memcpy(text_realloced[text_ptr.len..], text_segment);
            text_ptr.* = text_realloced;
        }
        pub inline fn feedCdataStart(ctx: *@This(), invalid: ?iksemel.Tokenizer.TokenSrc) !void {
            try ctx.appendNodeToChildren(.{ .text = blk: {
                const invalid_src = invalid orelse break :blk try ctx.arena.dupe(u8, "<![CDATA[");
                break :blk try ctx.arena.dupe(u8, ctx.tokenizer.getSrcString(invalid_src));
            } });
        }
        pub inline fn feedCdataEnd(ctx: *@This()) !void {
            const text_segment = "]]>";
            const text_ptr: *[]const u8 = &ctx.latestChild().text;
            const text_realloced = try ctx.arena.realloc(@constCast(text_ptr.*), text_ptr.len + text_segment.len);
            @memcpy(text_realloced[text_ptr.len..], text_segment);
            text_ptr.* = text_realloced;
        }
        pub inline fn feedCommentStart(ctx: *@This()) !void {
            _ = ctx;
        }
        pub inline fn feedCommentSegment(ctx: *@This(), segment: CommentSegment) !void {
            _ = ctx;
            _ = segment;
        }
        pub inline fn feedPiStart(ctx: *@This()) !void {
            _ = ctx;
        }
        pub inline fn feedPiData(ctx: *@This(), data_src: ?iksemel.Tokenizer.TokenSrc) !void {
            _ = ctx;
            _ = data_src;
        }
        pub inline fn feedPiEnd(ctx: *@This()) !void {
            _ = ctx;
        }
        pub inline fn feedReferenceStart(ctx: *@This()) !void {
            _ = ctx;
        }
        pub inline fn feedReferenceSegment(ctx: *@This(), tag_src: ?iksemel.Tokenizer.TokenSrc) !void {
            _ = ctx;
            _ = tag_src;
        }
        pub inline fn feedReferenceEnd(ctx: *@This(), valid: bool) !void {
            _ = ctx;
            _ = valid;
        }
        pub inline fn feedElementOpenStart(ctx: *@This()) !void {
            assert(ctx.element_tag_state == null);
            ctx.element_tag_state = .open;
            try ctx.appendNodeToChildren(.{ .element = .{
                .name = "",
                .attributes = &.{},
                .children = &.{},
            } });
            try ctx.parent_stack.append(ctx.arena, ctx.current);
            ctx.current = &ctx.latestChild().element;
        }
        pub inline fn feedElementCloseStart(ctx: *@This()) !void {
            assert(ctx.element_tag_state == null);
            ctx.element_tag_state = .close;
            try ctx.appendNodeToChildren(.{ .element_close = .{
                .name = "",
                .invalid_tokens = &.{},
            } });
        }
        pub inline fn feedElementTagNameSegment(ctx: *@This(), name: ?iksemel.Tokenizer.TokenSrc) !void {
            const name_src = name orelse return;
            const name_segment = ctx.tokenizer.getSrcString(name_src);
            const name_ptr: *[]const u8 = switch (ctx.element_tag_state.?) {
                .open => &ctx.current.name,
                .close => &ctx.latestChild().element_close.name,
            };
            const name_realloced = try ctx.arena.realloc(@constCast(name_ptr.*), name_ptr.len + name_segment.len);
            @memcpy(name_realloced[name_ptr.len..], name_segment);
            name_ptr.* = name_realloced;
        }
        pub inline fn feedElementTagEnd(ctx: *@This()) !void {
            defer ctx.element_tag_state = null;
            switch (ctx.element_tag_state.?) {
                .open => {},
                .close => ctx.current = ctx.parent_stack.popOrNull() orelse return,
            }
        }
        pub inline fn feedElementOpenEndInlineClose(ctx: *@This()) !void {
            assert(ctx.element_tag_state.? == .open);
            ctx.element_tag_state = null;
        }
        pub inline fn feedAttributeNameStart(ctx: *@This()) !void {
            const elem = ctx.current;
            const attrs_realloced: []Node.Attribute = try ctx.arena.realloc(@constCast(elem.attributes), elem.attributes.len + 1);
            attrs_realloced[attrs_realloced.len - 1] = .{
                .name = "",
                .eql = false,
                .value = null,
            };
            elem.attributes = attrs_realloced;
        }
        pub inline fn feedAttributeNameSegment(ctx: *@This(), name: ?iksemel.Tokenizer.TokenSrc) !void {
            const name_src = name orelse return;
            const name_segment = ctx.tokenizer.getSrcString(name_src);

            const elem = ctx.current;
            const attr: *Node.Attribute = @constCast(&elem.attributes[elem.attributes.len - 1]);

            const name_ptr: *[]const u8 = &attr.name;
            const name_realloced = try ctx.arena.realloc(@constCast(name_ptr.*), name_ptr.len + name_segment.len);
            @memcpy(name_realloced[name_ptr.len..], name_segment);
            name_ptr.* = name_realloced;
        }
        pub inline fn feedAttributeEquals(ctx: *@This()) !void {
            const elem = ctx.current;
            {
                const attr: *Node.Attribute = @constCast(&elem.attributes[elem.attributes.len - 1]);
                if (!attr.eql) {
                    attr.eql = true;
                    return;
                }
            }
            const attrs_realloced: []Node.Attribute = try ctx.arena.realloc(@constCast(elem.attributes), elem.attributes.len + 1);
            attrs_realloced[attrs_realloced.len - 1] = .{
                .name = "",
                .eql = true,
                .value = null,
            };
            elem.attributes = attrs_realloced;
        }
        pub inline fn feedAttributeValueStart(ctx: *@This(), quote_type: QuoteType) !void {
            _ = quote_type;
            const elem = ctx.current;
            {
                const attr: *Node.Attribute = @constCast(&elem.attributes[elem.attributes.len - 1]);
                if (attr.value == null) {
                    attr.value = "";
                    return;
                }
            }
            const attrs_realloced: []Node.Attribute = try ctx.arena.realloc(@constCast(elem.attributes), elem.attributes.len + 1);
            attrs_realloced[attrs_realloced.len - 1] = .{
                .name = "",
                .eql = true,
                .value = "",
            };
            elem.attributes = attrs_realloced;
        }
        pub inline fn feedAttributeValueSegment(ctx: *@This(), value: AttributeSegment) !void {
            const value_segment = switch (value) {
                .text => |tok_src| ctx.tokenizer.getSrcString(tok_src),
                .invalid_angle_bracket_left => "<",
                .end_quote => return,
            };

            const elem = ctx.current;
            const attr: *Node.Attribute = @constCast(&elem.attributes[elem.attributes.len - 1]);

            const value_ptr: *[]const u8 = &attr.value.?;
            const value_realloced = try ctx.arena.realloc(@constCast(value_ptr.*), value_ptr.len + value_segment.len);
            @memcpy(value_realloced[value_ptr.len..], value_segment);
            value_ptr.* = value_realloced;
        }
    };

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var ctx: Ctx = undefined;
    ctx = .{
        .tokenizer = &tokenizer,
        .arena = arena.allocator(),
        .current = &ctx.root,
    };

    try parseSliceTokenizer(&ctx, &tokenizer);
    try std.testing.expectEqualDeep(Ctx.Node.Element{
        .name = "",
        .attributes = &.{},
        .children = &[_]Ctx.Node{
            .{ .text = "\n" },
            .{
                .element = .{
                    .name = "foo",
                    .attributes = &.{},
                    .children = &.{
                        .{ .text = "\n  " },
                        .{
                            .element = .{
                                .name = "bar",
                                .attributes = &.{
                                    .{ .name = "fizz", .eql = true, .value = "buzz" },
                                },
                                .children = &.{
                                    .{ .text = "baz" },
                                    .{
                                        .element_close = .{
                                            .name = "bar",
                                            .invalid_tokens = &.{},
                                        },
                                    },
                                },
                            },
                        },
                        .{ .text = "\n" },
                        .{
                            .element_close = .{
                                .name = "foo",
                                .invalid_tokens = &.{},
                            },
                        },
                    },
                },
            },
            .{ .text = "\n" },
        },
    }, ctx.root);
}
