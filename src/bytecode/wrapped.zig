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
                } else if (comptime field.name.len >= "aload".len and std.mem.eql(u8, field.name[1 .. 1 + "load".len], "load")) {
                    return .{
                        .load_local = .{
                            .kind = BytecodePrimitive.fromShorthand(field.name[0]),
                            .index = if (field.name.len == "aload".len) @field(op, field.name) else comptime std.fmt.parseInt(u16, field.name["aload_".len..], 10) catch unreachable,
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

test "Wrapped: Load" {
    var iload_1_op = ops.Operation{ .iload_1 = {} };
    var iload_1_wrapped = WrappedOperation.wrap(iload_1_op);
    try std.testing.expectEqual(BytecodePrimitive.int, iload_1_wrapped.load_local.kind);
    try std.testing.expectEqual(@as(u16, 1), iload_1_wrapped.load_local.index);

    var iload_n_op = ops.Operation{ .iload = 12 };
    var iload_n_wrapped = WrappedOperation.wrap(iload_n_op);
    try std.testing.expectEqual(BytecodePrimitive.int, iload_n_wrapped.load_local.kind);
    try std.testing.expectEqual(@as(u16, 12), iload_n_wrapped.load_local.index);
}

pub const StoreLocalOperation = struct {
    kind: BytecodePrimitive,
    index: u16,
};

test "Wrapped: Store" {
    var istore_1_op = ops.Operation{ .istore_1 = {} };
    var istore_1_wrapped = WrappedOperation.wrap(istore_1_op);
    try std.testing.expectEqual(BytecodePrimitive.int, istore_1_wrapped.store_local.kind);
    try std.testing.expectEqual(@as(u16, 1), istore_1_wrapped.store_local.index);

    var istore_n_op = ops.Operation{ .istore = 12 };
    var istore_n_wrapped = WrappedOperation.wrap(istore_n_op);
    try std.testing.expectEqual(BytecodePrimitive.int, istore_n_wrapped.store_local.kind);
    try std.testing.expectEqual(@as(u16, 12), istore_n_wrapped.store_local.index);
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
}

pub const ReturnOperation = struct {
    kind: ?BytecodePrimitive,
};
