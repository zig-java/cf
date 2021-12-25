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

    pub fn fromShorthand(shorthand: u8) PrimitiveKind {
        return switch (shorthand) {
            'a' => .reference,
            'd' => .double,
            'f' => .float,
            'i' => .int,
            'l' => .long,
            's' => .short,
            else => @panic("No shorthand!"),
        };
    }

    pub fn toShorthand(self: PrimitiveKind) u8 {
        return switch (self) {
            .reference => 'a',
            .double => 'd',
            .float => 'f',
            .int => 'i',
            .long => 'l',
            .short => 's',
            else => @panic("No shorthand!"),
        };
    }
};

pub const BytecodePrimitiveValue = union(enum) {
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
        inline for (std.meta.fields(ops.Operation)) |field| {
            var opcode = @field(ops.Opcode, field.name);
            if (std.meta.activeTag(op) == opcode) {
                if (@enumToInt(opcode) >= 0x85 and @enumToInt(opcode) <= 0x93) {
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

pub const ConvertOperation = struct {
    from: BytecodePrimitive,
    to: BytecodePrimitive,
};
