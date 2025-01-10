const std = @import("std");
const main = @import("main.zig");
const parser = main.parser;
const ast = main.ast;

test "parse simple class" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    const source = "(Class ^name MyClass ^props { ^a (Property ^^required ^type Int) })";

    const result = parser.parseGeneSource(&allocator, source) catch @panic("parse error");
    try std.testing.expect(result.len == 1);

    // check it's a ClassDecl if your parser does that
    // e.g. match on union
    switch (result[0]) {
        .ClassDecl => |cd| {
            try std.testing.expectEqualStrings("MyClass", cd.name);
            try std.testing.expect(cd.props.len == 1);
        },
        else => @panic("expected ClassDecl node"),
    }
}
