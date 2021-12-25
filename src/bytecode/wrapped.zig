//! Actually useful bytecode typing
//! Note that even though index, etc. types are u8s, they're marked as u16s for wide compat.

const std = @import("std");
const ops = @import("ops.zig");

pub const BytecodePrimitive = enum {
    byte,
    char,
    short,
    int,
    long,
    float,
    double,
    reference,

    pub fn fromShorthand(shorthand: u8) BytecodePrimitive {
        return switch (shorthand) {
            'a' => .reference,
            'd' => .double,
            'f' => .float,
            'i' => .int,
            'l' => .long,
            's' => .short,
            'b' => .byte,
            else => @panic("No shorthand!"),
        };
    }

    pub fn toShorthand(self: BytecodePrimitive) u8 {
        return switch (self) {
            .reference => 'a',
            .double => 'd',
            .float => 'f',
            .int => 'i',
            .long => 'l',
            .short => 's',
            .byte => 'b',
            else => @panic("No shorthand!"),
        };
    }

    pub fn toType(self: BytecodePrimitive) type {
        return std.meta.TagPayload(BytecodePrimitiveValue, self);
    }
};

pub const BytecodePrimitiveValue = union(BytecodePrimitive) {
    byte: i8,
    char: u16,
    short: i16,
    int: i32,
    long: i64,
    float: f32,
    double: f64,
    reference: usize,
};

pub const WrappedOperation = union(enum) {
    load_local: LoadLocalOperation,
    store_local: StoreLocalOperation,
    convert: ConvertOperation,

    pub fn wrap(op: ops.Operation) WrappedOperation {
        @setEvalBranchQuota(10000);

        inline for (std.meta.fields(ops.Operation)) |field| {
            comptime var opcode = @field(ops.Opcode, field.name);
            if (std.meta.activeTag(op) == opcode) {
                if (comptime field.name.len >= "astore".len and std.mem.eql(u8, field.name[1 .. 1 + "store".len], "store")) {
                    return .{
                        .store_local = .{
                            .kind = BytecodePrimitive.fromShorthand(field.name[0]),
                            .index = if (field.name.len == "astore".len) @field(op, field.name) else comptime std.fmt.parseInt(u16, field.name["astore_".len..], 10) catch unreachable,
                        },
                    };
                } else if (comptime @enumToInt(opcode) >= 0x85 and @enumToInt(opcode) <= 0x93) {
                    return .{
                        .convert = .{
                            .from = BytecodePrimitive.fromShorthand(field.name[0]),
                            .to = BytecodePrimitive.fromShorthand(field.name[2]),
                        },
                    };
                } else {
                    @panic("Not implemented: " ++ field.name);
                }
            }
        }

        unreachable;
    }

    // TODO: Implement
    pub fn unwrap(self: WrappedOperation) ops.Operation {
        return self;
    }
};

pub const LoadLocalOperation = struct {
    kind: BytecodePrimitive,
    index: u16,
};

pub const StoreLocalOperation = struct {
    kind: BytecodePrimitive,
    index: u16,
};

test "Wrapped: Store" {
    var istore_op = ops.Operation{ .istore_3 = {} };
    _ = istore_op;
}

pub const ConvertOperation = struct {
    from: BytecodePrimitive,
    to: BytecodePrimitive,
};

test "Wrapped: Convert" {
    var i2b_op = ops.Operation{ .i2b = {} };
    var i2b_wrapped = WrappedOperation.wrap(i2b_op);
    try std.testing.expect(i2b_wrapped == .convert);
    try std.testing.expectEqual(BytecodePrimitive.int, i2b_wrapped.convert.from);
    try std.testing.expectEqual(BytecodePrimitive.byte, i2b_wrapped.convert.to);

    var i2l_op = ops.Operation{ .i2l = {} };
    var i2l_wrapped = WrappedOperation.wrap(i2l_op);
    try std.testing.expect(i2l_wrapped == .convert);
    try std.testing.expectEqual(BytecodePrimitive.int, i2l_wrapped.convert.from);
    try std.testing.expectEqual(BytecodePrimitive.long, i2l_wrapped.convert.to);

    // var l2i_wrapped = WrappedOperation{ .convert = .{ .from = .long, .to = .int } };
    // l2i_wrapped.
}
