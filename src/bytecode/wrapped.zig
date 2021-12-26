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
    reference: ?usize,
};

pub const WrappedOperation = union(enum) {
    load_from_constant_pool: LoadFromConstantPoolOperation,
    store_local: StoreLocalOperation,
    load_local: LoadLocalOperation,
    convert: ConvertOperation,
    @"return": ReturnOperation,

    pub fn wrap(op: ops.Operation) WrappedOperation {
        @setEvalBranchQuota(10000);

        inline for (std.meta.fields(ops.Operation)) |operation_field| {
            comptime var opcode = @field(ops.Opcode, operation_field.name);
            if (std.meta.activeTag(op) == opcode) {
                inline for (std.meta.fields(WrappedOperation)) |wo_field| {
                    if (comptime wo_field.field_type.matches(opcode, operation_field)) {
                        return @unionInit(WrappedOperation, wo_field.name, wo_field.field_type.wrap(op, opcode, operation_field));
                    }
                }

                @panic("Not implemented: " ++ operation_field.name);
            }
        }

        unreachable;
    }

    // TODO: Implement
    pub fn unwrap(self: WrappedOperation) ops.Operation {
        return self;
    }
};

pub const ConstantSize = enum {
    /// Everything else
    one,
    /// Floats and doubles
    two,
};

pub const LoadFromConstantPoolOperation = struct {
    size: ConstantSize,
    index: u16,

    fn matches(comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) bool {
        _ = operation_field;
        return opcode == .ldc or opcode == .ldc_w or opcode == .ldc2_w;
    }

    fn wrap(op: ops.Operation, comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) LoadFromConstantPoolOperation {
        _ = opcode;
        _ = operation_field;
        return .{
            .size = switch (op) {
                .ldc, .ldc_w => .one,
                .ldc2_w => .two,
                else => unreachable,
            },
            .index = switch (op) {
                .ldc => |b| b,
                .ldc_w => |b| b,
                .ldc2_w => |b| b,
                else => unreachable,
            },
        };
    }
};

test "Wrapped: Load From Constant Pool" {
    var ldc_op = ops.Operation{ .ldc = 7 };
    var ldc_wrapped = WrappedOperation.wrap(ldc_op);

    try std.testing.expectEqual(ConstantSize.one, ldc_wrapped.load_from_constant_pool.size);
    try std.testing.expectEqual(@as(u16, 7), ldc_wrapped.load_from_constant_pool.index);

    var ldc2_op = ops.Operation{ .ldc2_w = 7 };
    var ldc2_wrapped = WrappedOperation.wrap(ldc2_op);

    try std.testing.expectEqual(ConstantSize.two, ldc2_wrapped.load_from_constant_pool.size);
    try std.testing.expectEqual(@as(u16, 7), ldc2_wrapped.load_from_constant_pool.index);
}

pub const StoreLocalOperation = struct {
    kind: BytecodePrimitive,
    index: u16,

    fn matches(comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) bool {
        _ = opcode;
        return operation_field.name.len >= "astore".len and std.mem.eql(u8, operation_field.name[1 .. 1 + "store".len], "store");
    }

    fn wrap(op: ops.Operation, comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) StoreLocalOperation {
        _ = opcode;
        return .{
            .kind = BytecodePrimitive.fromShorthand(operation_field.name[0]),
            .index = if (operation_field.name.len == "astore".len) @field(op, operation_field.name) else comptime std.fmt.parseInt(u16, operation_field.name["astore_".len..], 10) catch unreachable,
        };
    }
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

pub const LoadLocalOperation = struct {
    kind: BytecodePrimitive,
    index: u16,

    fn matches(comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) bool {
        _ = opcode;
        return operation_field.name.len >= "aload".len and std.mem.eql(u8, operation_field.name[1 .. 1 + "load".len], "load");
    }

    fn wrap(op: ops.Operation, comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) LoadLocalOperation {
        _ = opcode;
        return .{
            .kind = BytecodePrimitive.fromShorthand(operation_field.name[0]),
            .index = if (operation_field.name.len == "aload".len) @field(op, operation_field.name) else comptime std.fmt.parseInt(u16, operation_field.name["aload_".len..], 10) catch unreachable,
        };
    }
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

pub const ConvertOperation = struct {
    from: BytecodePrimitive,
    to: BytecodePrimitive,

    fn matches(comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) bool {
        _ = operation_field;
        return @enumToInt(opcode) >= 0x85 and @enumToInt(opcode) <= 0x93;
    }

    fn wrap(op: ops.Operation, comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) ConvertOperation {
        _ = op;
        _ = opcode;
        return .{
            .from = BytecodePrimitive.fromShorthand(operation_field.name[0]),
            .to = BytecodePrimitive.fromShorthand(operation_field.name[2]),
        };
    }
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

    fn matches(comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) bool {
        _ = operation_field;
        return @enumToInt(opcode) >= 0xac and @enumToInt(opcode) <= 0xb1;
    }

    fn wrap(op: ops.Operation, comptime opcode: ops.Opcode, comptime operation_field: std.builtin.TypeInfo.UnionField) ReturnOperation {
        _ = op;
        _ = opcode;
        return .{
            .kind = if (operation_field.name[0] == 'r') null else BytecodePrimitive.fromShorthand(operation_field.name[0]),
        };
    }
};

test "Wrapped: Return" {
    var return_op = ops.Operation{ .@"return" = {} };
    var return_wrapped = WrappedOperation.wrap(return_op);
    try std.testing.expectEqual(@as(?BytecodePrimitive, null), return_wrapped.@"return".kind);

    var areturn_op = ops.Operation{ .areturn = {} };
    var areturn_wrapped = WrappedOperation.wrap(areturn_op);
    try std.testing.expectEqual(BytecodePrimitive.reference, areturn_wrapped.@"return".kind.?);
}
