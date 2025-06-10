const std = @import("std");
const lir = @import("lir.zig");

pub fn serialize(writer: anytype, program: lir.LIR, title: []const u8) !void {
    try writer.print("=== {s} ===\n", .{title});
    
    for (program.functions.items, 0..) |func, i| {
        try writer.print("Function {d}: {s}\n", .{ i, func.name });
        try writer.print("  Params: {d}\n", .{func.param_count});
        try writer.print("  Registers: {d}\n", .{func.register_count});
        
        // Print labels
        if (func.labels.items.len > 0) {
            try writer.print("  Labels:\n", .{});
            for (func.labels.items) |label| {
                try writer.print("    {s}: @{d}\n", .{ label.name, label.position });
            }
        }
        
        // Print instructions
        try writer.print("  Instructions:\n", .{});
        for (func.instructions.items, 0..) |instr, idx| {
            try writer.print("    {d:4}: ", .{idx});
            try serializeInstruction(writer, instr);
            try writer.print("\n", .{});
        }
        try writer.print("\n", .{});
    }
}

fn serializeInstruction(writer: anytype, instr: lir.LIR.Instruction) !void {
    switch (instr) {
        .LoadConst => |load_const| {
            try writer.print("LoadConst r{d}, ", .{load_const.dest});
            try serializeValue(writer, load_const.val);
        },
        .LoadNil => |load_nil| {
            try writer.print("LoadNil r{d}", .{load_nil.dest});
        },
        .LoadVariable => |load_var| {
            try writer.print("LoadVariable r{d}, ", .{load_var.dest});
            try serializeValue(writer, load_var.name);
        },
        .Add => |add| {
            try writer.print("Add r{d}, r{d}, r{d}", .{ add.dest, add.left, add.right });
        },
        .Sub => |sub| {
            try writer.print("Sub r{d}, r{d}, r{d}", .{ sub.dest, sub.left, sub.right });
        },
        .Mul => |mul| {
            try writer.print("Mul r{d}, r{d}, r{d}", .{ mul.dest, mul.left, mul.right });
        },
        .Div => |div| {
            try writer.print("Div r{d}, r{d}, r{d}", .{ div.dest, div.left, div.right });
        },
        .Eq => |eq| {
            try writer.print("Eq r{d}, r{d}, r{d}", .{ eq.dest, eq.left, eq.right });
        },
        .Lt => |lt| {
            try writer.print("Lt r{d}, r{d}, r{d}", .{ lt.dest, lt.left, lt.right });
        },
        .Le => |le| {
            try writer.print("Le r{d}, r{d}, r{d}", .{ le.dest, le.left, le.right });
        },
        .Gt => |gt| {
            try writer.print("Gt r{d}, r{d}, r{d}", .{ gt.dest, gt.left, gt.right });
        },
        .Ge => |ge| {
            try writer.print("Ge r{d}, r{d}, r{d}", .{ ge.dest, ge.left, ge.right });
        },
        .And => |and_op| {
            try writer.print("And r{d}, r{d}, r{d}", .{ and_op.dest, and_op.left, and_op.right });
        },
        .Or => |or_op| {
            try writer.print("Or r{d}, r{d}, r{d}", .{ or_op.dest, or_op.left, or_op.right });
        },
        .Not => |not_op| {
            try writer.print("Not r{d}, r{d}", .{ not_op.dest, not_op.src });
        },
        .Jump => |jump| {
            try writer.print("Jump L{d}", .{jump.target});
        },
        .JumpIf => |jump_if| {
            try writer.print("JumpIf r{d}, L{d}", .{ jump_if.cond, jump_if.target });
        },
        .JumpIfNot => |jump_if_not| {
            try writer.print("JumpIfNot r{d}, L{d}", .{ jump_if_not.cond, jump_if_not.target });
        },
        .Call => |call| {
            try writer.print("Call r{d}, r{d}, [", .{ call.dest, call.func });
            for (call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("r{d}", .{arg});
            }
            try writer.print("]", .{});
        },
        .TailCall => |tail_call| {
            try writer.print("TailCall r{d}, [", .{tail_call.func});
            for (tail_call.args.items, 0..) |arg, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("r{d}", .{arg});
            }
            try writer.print("]", .{});
        },
        .Return => |ret| {
            if (ret.value) |val| {
                try writer.print("Return r{d}", .{val});
            } else {
                try writer.print("Return", .{});
            }
        },
        .NewObject => |new_obj| {
            try writer.print("NewObject r{d}, class{d}", .{ new_obj.dest, new_obj.class_id });
        },
        .GetField => |get_field| {
            try writer.print("GetField r{d}, r{d}, field{d}", .{ get_field.dest, get_field.obj, get_field.field_id });
        },
        .SetField => |set_field| {
            try writer.print("SetField r{d}, field{d}, r{d}", .{ set_field.obj, set_field.field_id, set_field.value });
        },
        .GetMethod => |get_method| {
            try writer.print("GetMethod r{d}, r{d}, method{d}", .{ get_method.dest, get_method.obj, get_method.method_id });
        },
        .CallMethod => |call_method| {
            try writer.print("CallMethod r{d}, r{d}, method{d}, [", .{ call_method.dest, call_method.obj, call_method.method_id });
            for (call_method.args.items, 0..) |arg, i| {
                if (i > 0) try writer.print(", ", .{});
                try writer.print("r{d}", .{arg});
            }
            try writer.print("]", .{});
        },
        .NewArray => |new_array| {
            try writer.print("NewArray r{d}, r{d}", .{ new_array.dest, new_array.size });
        },
        .GetElement => |get_elem| {
            try writer.print("GetElement r{d}, r{d}, r{d}", .{ get_elem.dest, get_elem.arr, get_elem.idx });
        },
        .SetElement => |set_elem| {
            try writer.print("SetElement r{d}, r{d}, r{d}", .{ set_elem.arr, set_elem.idx, set_elem.value });
        },
        .ArrayLen => |array_len| {
            try writer.print("ArrayLen r{d}, r{d}", .{ array_len.dest, array_len.arr });
        },
        .TypeCheck => |type_check| {
            try writer.print("TypeCheck r{d}, r{d}, type{d}", .{ type_check.dest, type_check.value, type_check.type_id });
        },
        .Cast => |cast| {
            try writer.print("Cast r{d}, r{d}, type{d}", .{ cast.dest, cast.value, cast.type_id });
        },
        .Move => |move| {
            try writer.print("Move r{d}, r{d}", .{ move.dest, move.src });
        },
        .LoadIC => |load_ic| {
            try writer.print("LoadIC r{d}, cache{d}", .{ load_ic.dest, load_ic.cache_id });
        },
        .StoreIC => |store_ic| {
            try writer.print("StoreIC cache{d}, r{d}", .{ store_ic.cache_id, store_ic.value });
        },
    }
}

fn serializeValue(writer: anytype, value: anytype) !void {
    _ = value; // Mark as unused
    // This is a placeholder - we need to implement Value serialization
    // For now, just print a generic representation
    try writer.print("{{value}}", .{});
}