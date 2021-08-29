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
                        var value = try @field(d.field_type, "decode")(constant_pool, allocator, fbs.reader(), attribute_name_index, attribute_length);
                        break :z value;
                    });
                };
        }

        return .unknown;
    }

    pub fn encode(self: AttributeInfo, writer: anytype) !void {
        if (self == .unknown) return;

        inline for (std.meta.fields(AttributeInfo)) |d| {
            if (@enumToInt(self) == d.value) {
                try @field(@field(self, d.name), "encode")(writer);
            }
        }

        unreachable;
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

    pub fn encode(self: ExceptionTableEntry, writer: anytype) !void {
        try writer.writeIntBig(u16, self.start_pc);
        try writer.writeIntBig(u16, self.end_pc);
        try writer.writeIntBig(u16, self.handler_pc);
        try writer.writeIntBig(u16, self.catch_pc);
    }
};

pub const CodeAttribute = struct {
    constant_pool: *const ConstantPool,
    attribute_name_index: u16,
    attribute_length: u32,

    max_stack: u16,
    max_locals: u16,

    code: []u8,
    exception_table: std.ArrayList(ExceptionTableEntry),

    attributes: std.ArrayList(AttributeInfo),

    pub fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype, attribute_name_index: u16, attribute_length: u32) !CodeAttribute {
        var max_stack = try reader.readIntBig(u16);
        var max_locals = try reader.readIntBig(u16);

        var code_length = try reader.readIntBig(u32);
        var code = try allocator.alloc(u8, code_length);
        _ = try reader.readAll(code);

        var exception_table = try std.ArrayList(ExceptionTableEntry).initCapacity(allocator, try reader.readIntBig(u16));
        for (exception_table.items) |*et| et.* = try ExceptionTableEntry.decode(reader);

        // TODO: Fix this awful, dangerous, slow hack
        var attributes_length = try reader.readIntBig(u16);
        var attributes_index: usize = 0;
        var attributes = std.ArrayList(AttributeInfo).init(allocator);
        while (attributes_index < attributes_length) {
            var decoded = try AttributeInfo.decode(constant_pool, allocator, reader);
            if (decoded == .unknown) {
                attributes_length -= 1;
                continue;
            }
            try attributes.append(decoded);
        }

        return CodeAttribute{
            .constant_pool = constant_pool,
            .attribute_name_index = attribute_name_index,
            .attribute_length = attribute_length,

            .max_stack = max_stack,
            .max_locals = max_locals,

            .code = code,
            .exception_table = exception_table,

            .attributes = attributes,
        };
    }

    pub fn encode(self: CodeAttribute, writer: anytype) !void {
        try writer.writeIntBig(u16, self.max_stack);
        try writer.writeIntBig(u16, self.max_locals);

        try writer.writeIntBig(u32, self.code.len);
        try writer.writeAll(self.code);

        try writer.writeIntBig(u16, self.exception_table.len);
        for (self.exception_table) |et| try et.encode(writer);

        try writer.writeIntBig(u16, self.attributes.len);
        for (self.attributes) |at| try at.encode(writer);
    }
};
