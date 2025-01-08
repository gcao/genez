const std = @import("std");
const parser = @import("../src/parser.zig");
const ast = @import("../src/ast.zig");

test "parse simple class" {
    var alloc = std.heap.page_allocator;
    const source = "(Class ^name MyClass ^props { ^a (Property ^^required ^type Int) })";

    const result = parser.parseGeneSource(alloc, source) catch @panic("parse error");
    std.testing.expect(result.len == 1);

    // check it's a ClassDecl if your parser does that
    // e.g. match on union
    switch (result[0]) {
        .ClassDecl => |cd| {
            std.testing.expectEqualStrings("MyClass", cd.name);
            std.testing.expect(cd.props.len == 1);
        },
        else => @panic("expected ClassDecl node"),
    }
}
