pub const FullScanner = Scanner(null);
pub inline fn fullScanner() FullScanner {
    return .{ .mbr = .{} };
}

pub fn StreamScanner(comptime Reader: type) type {
    return Scanner(Reader);
}
pub inline fn streamScanner(reader: anytype, read_buffer: []u8) StreamScanner(@TypeOf(reader)) {
    return .{
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buffer,
        },
    };
}

pub const IdKind = enum {
    public,
    system,
};

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
};

pub fn Scanner(comptime MaybeReader: ?type) type {
    return struct {
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
        state: State = .start,
        const Self = @This();

        pub const SrcError = if (MaybeReader) |Reader| Reader.Error else error{};

        pub const TokenizerAPI = if (MaybeReader != null) Tokenizer.Stream else Tokenizer.Full;
        pub const Src = if (MaybeReader != null) []const u8 else Tokenizer.Range;

        /// This must be called first.
        /// Expects that the last token returned by `tokenizer` was `.angle_bracket_left_bang`, followed
        /// by a `.tag_token`, whose source satisfies `dtd.MarkupDeclKind.fromString(source) == .notation`.
        /// See `nameNextSegment` next.
        pub fn expectName(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!void {
            assert(scanner.state == .start);
            defer assert(scanner.state != .start);
            errdefer scanner.state = .err;

            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .tag_token => {},
            }
            scanner.state = .name;
        }

        /// This must be called second, and then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the declared notation's name.
        /// See `idKind` next.
        pub fn nameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            scanner.state = .detect_id_kind;
            return null;
        }

        /// This must be called after `scanner.nameNextSegment(tokenizer) = null`.
        /// If the returned value is `.system`, it is an External ID marked SYSTEM.
        /// If the returned value is `.public`, it is either an External ID marked
        /// PUBLIC, or a Public ID; which one it is will be determined by whether
        /// the Pubid Literal is followed by a System Literal; if it is, then the
        /// former applies, otherwise the latter applies.
        /// See `pubidLiteralNextSegment`, and `systemLiteralNextSegment` next.
        pub fn idKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!IdKind {
            assert(scanner.state == .detect_id_kind);
            defer assert(scanner.state != .detect_id_kind);
            errdefer scanner.state = .err;

            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .tag_token => {},
            }

            const maybe_pub_or_sys = try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr, enum { PUBLIC, SYSTEM });
            const id_kind: IdKind = switch (maybe_pub_or_sys orelse return ScanError.UnexpectedToken) {
                .PUBLIC => .public,
                .SYSTEM => .system,
            };

            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            const quote_type: xml.prod.QuoteType = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                inline //
                .quote_single,
                .quote_double,
                => |narrow_tt| comptime xml.prod.QuoteType.fromTokenTypeNarrow(narrow_tt).?,
            };
            const is_empty: bool = switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,

                .text_data,
                => false,

                .quote_single,
                .quote_double,
                => true,
            };
            scanner.state = switch (id_kind) {
                .public => if (is_empty) .pubid_literal_empty else switch (quote_type) {
                    .single => .pubid_literal_sq,
                    .double => .pubid_literal_dq,
                },
                .system => if (is_empty) .system_literal_empty else switch (quote_type) {
                    .single => .system_literal_sq,
                    .double => .system_literal_dq,
                },
            };
            return id_kind;
        }

        /// This must be called after `scanner.idKind(tokenizer) = .public`, and
        /// then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the content of the Pubid Literal.
        /// See `systemLiteralPresent` next.
        pub fn pubidLiteralNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            errdefer scanner.state = .err;
            switch (scanner.state) {
                .pubid_literal_sq,
                .pubid_literal_dq,
                => |state_tag| {
                    const quote_type: xml.prod.QuoteType = switch (state_tag) {
                        .pubid_literal_sq => .single,
                        .pubid_literal_dq => .double,
                        else => unreachable,
                    };
                    if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) |segment| {
                        return segment;
                    }
                    switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .text_data => unreachable,

                        .quote_single,
                        .quote_double,
                        => {},
                    }
                },

                .pubid_literal_empty => {},
                else => unreachable,
            }

            scanner.state = .detect_system_literal;
            return null;
        }

        /// This must be called after `scanner.pubidLiteralNextSegment(tokenizer) = null`.
        /// If this returns true, there is a System Literal following the Pubid Literal.
        /// If this returns false, we have reached the end of the notation declaration.
        /// See `systemLiteralNextSegment` next.
        pub fn systemLiteralPresent(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!bool {
            assert(scanner.state == .detect_system_literal);

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .angle_bracket_right => {
                    scanner.state = .end;
                    return false;
                },
                .tag_whitespace => {},
            }
            try parse_helper.skipWhitespaceSrcUnchecked(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr);

            const quote_type: xml.prod.QuoteType = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .angle_bracket_right => {
                    scanner.state = .end;
                    return false;
                },

                inline //
                .quote_single,
                .quote_double,
                => |narrow_tt| comptime xml.prod.QuoteType.fromTokenTypeNarrow(narrow_tt).?,
            };

            scanner.state = switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .text_data,
                => switch (quote_type) {
                    .single => .system_literal_sq,
                    .double => .system_literal_dq,
                },
                .quote_single,
                .quote_double,
                => .system_literal_empty,
            };
            return true;
        }

        /// This must be called after `scanner.idKind(tokenizer) = .system`, or
        /// after `scanner.systemLiteralPresent(tokenizer) = true`, and then until
        /// it returns null.
        /// The returned source segments should be concatenated
        /// in order to obtain the content of the System Literal.
        /// After this returns null, we have reached the end of the
        /// notation declaration.
        pub fn systemLiteralNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            errdefer scanner.state = .err;
            switch (scanner.state) {
                .system_literal_sq,
                .system_literal_dq,
                => |state_tag| {
                    const quote_type: xml.prod.QuoteType = switch (state_tag) {
                        .system_literal_sq => .single,
                        .system_literal_dq => .double,
                        else => unreachable,
                    };
                    if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) |segment| {
                        return segment;
                    }
                    switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .text_data => unreachable,

                        .quote_single,
                        .quote_double,
                        => {},
                    }
                },

                .system_literal_empty => {},
                else => unreachable,
            }

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .angle_bracket_right => {},
            }

            scanner.state = .end;
            return null;
        }

        fn expectAndSkipTagWhitespace(scanner: *const Self, tokenizer: *Tokenizer) (SrcError || ScanError)!void {
            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .markup, MaybeReader, scanner.mbr)) |tt| switch (tt) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
            };
        }

        const State = enum {
            start,
            end,
            err,

            name,
            detect_id_kind,

            pubid_literal_sq,
            pubid_literal_dq,
            pubid_literal_empty,

            detect_system_literal,

            system_literal_sq,
            system_literal_dq,
            system_literal_empty,
        };
    };
}

const ScannerTestValues = struct {
    name: []const u8,
    id_kind: IdKind,
    pubid_literal: ?[]const u8,
    system_literal: ?[]const u8,

    fn deinit(stv: ScannerTestValues, allocator: std.mem.Allocator) void {
        allocator.free(stv.name);
        allocator.free(stv.pubid_literal orelse "");
        allocator.free(stv.system_literal orelse "");
    }

    pub fn format(
        stv: ScannerTestValues,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt_str;
        _ = options;
        try writer.print("<!NOTATION {} ", .{std.zig.fmtEscapes(stv.name)});
        try writer.writeAll(switch (stv.id_kind) {
            .public => "PUBLIC",
            .system => "SYSTEM",
        });
        if (stv.pubid_literal) |pubid| {
            const quote: u8 = if (std.mem.indexOfScalar(u8, pubid, '\'') != null) '\"' else '\'';
            try writer.writeByte(' ');
            try writer.writeByte(quote);
            try writer.print("{}", .{std.zig.fmtEscapes(pubid)});
            try writer.writeByte(quote);
        }
        if (stv.system_literal) |system| {
            const quote: u8 = if (std.mem.indexOfScalar(u8, system, '\'') != null) '\"' else '\'';
            try writer.writeByte(' ');
            try writer.writeByte(quote);
            try writer.print("{}", .{std.zig.fmtEscapes(system)});
            try writer.writeByte(quote);
        }
        try writer.writeByte('>');
    }

    fn expectEqual(expected: ScannerTestValues, actual: ScannerTestValues) !void {
        errdefer std.log.err("\nExpected:\n{}\nGot:\n{}", .{ expected, actual });
        try std.testing.expectEqualStrings(expected.name, actual.name);
        try std.testing.expectEqual(expected.id_kind, actual.id_kind);
        try test_helper.expectEqualStringOrErrOrNull(expected.pubid_literal, actual.pubid_literal);
        try test_helper.expectEqualStringOrErrOrNull(expected.system_literal, actual.system_literal);
    }

    fn parse(
        allocator: std.mem.Allocator,
        comptime MaybeReader: ?type,
        tokenizer: *Scanner(MaybeReader).TokenizerAPI,
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    ) !ScannerTestValues {
        const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
        var scanner = if (MaybeReader != null) streamScanner(mbr.reader, mbr.read_buffer) else fullScanner();

        try scanner.expectName(tokenizer);

        const name: []const u8 = str: {
            var name = std.ArrayList(u8).init(allocator);
            defer name.deinit();

            while (try scanner.nameNextSegment(tokenizer)) |segment| {
                const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                try name.appendSlice(segment_str);
            }

            break :str try name.toOwnedSlice();
        };
        errdefer allocator.free(name);

        const id_kind = try scanner.idKind(tokenizer);
        const pubid_lit: ?[]const u8, const system_lit: ?[]const u8 = switch (id_kind) {
            .public => blk: {
                const pubid_lit: ?[]const u8 = pubid: {
                    var pubid_lit = std.ArrayList(u8).init(allocator);
                    defer pubid_lit.deinit();

                    while (try scanner.pubidLiteralNextSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try pubid_lit.appendSlice(segment_str);
                    }
                    break :pubid try pubid_lit.toOwnedSlice();
                };

                const system_lit: ?[]const u8 = if (try scanner.systemLiteralPresent(tokenizer)) system: {
                    var system_lit = std.ArrayList(u8).init(allocator);
                    defer system_lit.deinit();

                    while (try scanner.systemLiteralNextSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try system_lit.appendSlice(segment_str);
                    }
                    break :system try system_lit.toOwnedSlice();
                } else null;

                break :blk .{ pubid_lit, system_lit };
            },
            .system => blk: {
                const system_lit: ?[]const u8 = system: {
                    var system_lit = std.ArrayList(u8).init(allocator);
                    defer system_lit.deinit();

                    while (try scanner.systemLiteralNextSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try system_lit.appendSlice(segment_str);
                    }
                    break :system try system_lit.toOwnedSlice();
                };
                break :blk .{ null, system_lit };
            },
        };
        errdefer comptime unreachable;

        return .{
            .name = name,
            .id_kind = id_kind,
            .pubid_literal = pubid_lit,
            .system_literal = system_lit,
        };
    }
};

fn testScanner(
    buffer_sizes: []const usize,
    /// Should begin with '<!ENTITY'
    src: []const u8,
    expected: ScannerTestValues,
) !void {
    {
        const max_buffer = try std.testing.allocator.alloc(u8, std.mem.max(usize, buffer_sizes));
        defer std.testing.allocator.free(max_buffer);

        for (buffer_sizes) |buffer_size| {
            const read_buffer = max_buffer[0..buffer_size];
            var fbs = std.io.fixedBufferStream(src);
            var tokenizer = Tokenizer.initStream();

            const Reader = @TypeOf(fbs).Reader;
            const mbr: parse_helper.MaybeBufferedReader(Reader) = .{
                .reader = fbs.reader(),
                .read_buffer = read_buffer,
            };

            try expectNotationDeclStart(Reader, &tokenizer.stream, mbr);
            const actual = try ScannerTestValues.parse(std.testing.allocator, Reader, &tokenizer.stream, mbr);
            defer actual.deinit(std.testing.allocator);
            try expected.expectEqual(actual);
        }
    }

    var tokenizer = Tokenizer.initFull(src);

    try expectNotationDeclStart(null, &tokenizer.full, .{});
    const actual = try ScannerTestValues.parse(std.testing.allocator, null, &tokenizer.full, .{});
    defer actual.deinit(std.testing.allocator);
    try expected.expectEqual(actual);
}

fn expectNotationDeclStart(
    comptime MaybeReader: ?type,
    tokenizer: *Scanner(MaybeReader).TokenizerAPI,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
    try std.testing.expectEqual(.angle_bracket_left_bang, try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    try std.testing.expectEqual(.tag_token, try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    var bstr: std.BoundedArray(u8, xml.dtd.MarkupDeclKind.max_str_len) = .{};
    while (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, mbr)) |segment| {
        const segment_str: []const u8 = if (MaybeReader != null) segment else segment.toStr(src);
        bstr.appendSlice(segment_str) catch return error.TestExpectedEqual;
    }
    errdefer std.log.err("Actual: '{}'", .{std.zig.fmtEscapes(bstr.constSlice())});
    try std.testing.expectEqual(.notation, xml.dtd.MarkupDeclKind.fromString(bstr.constSlice()));
}

test "basic" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };
    try testScanner(&buf_sizes, "<!NOTATION foo SYSTEM 'baz'>", .{
        .name = "foo",
        .id_kind = .system,
        .pubid_literal = null,
        .system_literal = "baz",
    });
    try testScanner(&buf_sizes, "<!NOTATION foo PUBLIC 'bar'>", .{
        .name = "foo",
        .id_kind = .public,
        .pubid_literal = "bar",
        .system_literal = null,
    });
    try testScanner(&buf_sizes, "<!NOTATION foo PUBLIC 'bar' 'baz'>", .{
        .name = "foo",
        .id_kind = .public,
        .pubid_literal = "bar",
        .system_literal = "baz",
    });
}

const std = @import("std");
const assert = std.debug.assert;

const xml = @import("../iksemel.zig");
const Tokenizer = xml.Tokenizer;

const parse_helper = @import("../parse_helper.zig");
const test_helper = @import("../test_helper.zig");
