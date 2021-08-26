const std = @import("std");
const ConstantPool = @import("ConstantPool.zig");

const AttributeMap = std.ComptimeStringMap([]const u8, .{.{ "Code", "code" }});

// TODO: Implement all attribute types
pub const AttributeInfo = union(enum) {
    code: CodeAttribute,
    unknown: void,

    pub fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype) anyerror!AttributeInfo {
        var attribute_name_index = try reader.readIntBig(u16);
        var attribute_length = try reader.readIntBig(u32);

        var info = try allocator.alloc(u8, attribute_length);
        _ = try reader.readAll(info);

        var fbs = std.io.fixedBufferStream(info);
        var name = constant_pool.get(attribute_name_index).utf8.bytes;

        inline for (std.meta.fields(AttributeInfo)) |d| {
            if (AttributeMap.get(name) != null)
                if (std.mem.eql(u8, AttributeMap.get(name).?, d.name)) {
                    return @unionInit(AttributeInfo, d.name, if (d.field_type == void) {} else z: {
                        var value = try @field(d.field_type, "decode")(constant_pool, allocator, fbs.reader(), attribute_name_index);
                        break :z value;
                    });
                };
        }

        return .unknown;
    }
};

pub const ExceptionTableEntry = packed struct {
    /// Where the exception handler becomes active (inclusive)
    start_pc: u16,
    /// Where it becomes inactive (exclusive)
    end_pc: u16,
    /// Start of handler
    handler_pc: u16,
    /// Index into constant pool
    catch_type: u16,

    // TODO: Fix this extremely bad, nasty, dangerous code!!
    pub fn decode(reader: anytype) !ExceptionTableEntry {
        return try reader.readStruct(ExceptionTableEntry);
    }
};

pub const CodeAttribute = struct {
    constant_pool: *const ConstantPool,
    attribute_name_index: u16,

    max_stack: u16,
    max_locals: u16,

    code: []u8,
    exception_table: []ExceptionTableEntry,

    attributes: []AttributeInfo,

    pub fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype, attribute_name_index: u16) !CodeAttribute {
        var max_stack = try reader.readIntBig(u16);
        var max_locals = try reader.readIntBig(u16);

        var code_length = try reader.readIntBig(u32);
        var code = try allocator.alloc(u8, code_length);
        _ = try reader.readAll(code);

        var exception_table_length = try reader.readIntBig(u16);
        var exception_table = try allocator.alloc(ExceptionTableEntry, exception_table_length);
        for (exception_table) |*et| et.* = try ExceptionTableEntry.decode(reader);

        var attributes_count = try reader.readIntBig(u16);
        var attributes = try allocator.alloc(AttributeInfo, attributes_count);
        for (attributes) |*at| at.* = try AttributeInfo.decode(constant_pool, allocator, reader);

        return CodeAttribute{
            .constant_pool = constant_pool,
            .attribute_name_index = attribute_name_index,

            .max_stack = max_stack,
            .max_locals = max_locals,

            .code = code,
            .exception_table = exception_table,

            .attributes = attributes,
        };
    }
};
