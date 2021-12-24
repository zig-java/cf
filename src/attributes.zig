const std = @import("std");
const ConstantPool = @import("ConstantPool.zig");

const logger = std.log.scoped(.cf_attributes);

// TODO: Rearrange for performance

const AttributeMap = std.ComptimeStringMap([]const u8, .{
    .{ "Code", "code" },
});

// TODO: Implement all attribute types
pub const AttributeInfo = union(enum) {
    code: CodeAttribute,
    unknown: void,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) anyerror!AttributeInfo {
        var attribute_name_index = try reader.readIntBig(u16);
        var attribute_length = try reader.readIntBig(u32);

        var info = try allocator.alloc(u8, attribute_length);
        defer allocator.free(info);
        _ = try reader.readAll(info);

        var fbs = std.io.fixedBufferStream(info);
        var name = constant_pool.get(attribute_name_index).utf8.bytes;

        inline for (std.meta.fields(AttributeInfo)) |d| {
            if (AttributeMap.get(name) != null)
                if (std.mem.eql(u8, AttributeMap.get(name).?, d.name)) {
                    return @unionInit(AttributeInfo, d.name, if (d.field_type == void) {} else z: {
                        var value = try @field(d.field_type, "decode")(constant_pool, allocator, fbs.reader());
                        break :z value;
                    });
                };
        }

        logger.err("Could not decode attribute: {s}", .{name});
        return .unknown;
    }

    pub fn calcAttrLen(self: AttributeInfo) u32 {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.field_type == void) continue;

            if (std.mem.eql(u8, @tagName(std.meta.activeTag(self)), field.name)) {
                return @field(self, field.name).calcAttrLen();
            }
        }

        unreachable;
    }

    pub fn deinit(self: AttributeInfo) void {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.field_type == void) continue;

            if (std.mem.eql(u8, @tagName(std.meta.activeTag(self)), field.name)) {
                return @field(self, field.name).deinit();
            }
        }

        unreachable;
    }

    pub fn encode(self: AttributeInfo, writer: anytype) !void {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.field_type == void) continue;

            if (std.mem.eql(u8, @tagName(std.meta.activeTag(self)), field.name)) {
                var attr = @field(self, field.name);

                try attr.encode(writer);

                try writer.writeIntBig(u16, try attr.constant_pool.locateUtf8Entry(@field(field.field_type, "name")));
                try writer.writeIntBig(u32, attr.calcAttrLen());
            }
        }
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

    pub fn decode(reader: anytype) !ExceptionTableEntry {
        var entry: ExceptionTableEntry = undefined;
        entry.start_pc = try reader.readIntBig(u16);
        entry.end_pc = try reader.readIntBig(u16);
        entry.handler_pc = try reader.readIntBig(u16);
        entry.catch_type = try reader.readIntBig(u16);
        return entry;
    }

    pub fn calcAttrLen(self: ExceptionTableEntry) u32 {
        _ = self;
        return 2 * 4;
    }

    pub fn encode(self: ExceptionTableEntry, writer: anytype) !void {
        try writer.writeIntBig(u16, self.start_pc);
        try writer.writeIntBig(u16, self.end_pc);
        try writer.writeIntBig(u16, self.handler_pc);
        try writer.writeIntBig(u16, self.catch_type);
    }
};

pub const CodeAttribute = struct {
    pub const name = "Code";

    constant_pool: *ConstantPool,

    max_stack: u16,
    max_locals: u16,

    code: std.ArrayList(u8),
    exception_table: std.ArrayList(ExceptionTableEntry),

    attributes: std.ArrayList(AttributeInfo),

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !CodeAttribute {
        var max_stack = try reader.readIntBig(u16);
        var max_locals = try reader.readIntBig(u16);

        var code_length = try reader.readIntBig(u32);
        var code = try std.ArrayList(u8).initCapacity(allocator, code_length);
        code.items.len = code_length;
        _ = try reader.readAll(code.items);

        var exception_table_len = try reader.readIntBig(u16);
        var exception_table = try std.ArrayList(ExceptionTableEntry).initCapacity(allocator, exception_table_len);
        exception_table.items.len = exception_table_len;
        for (exception_table.items) |*et| et.* = try ExceptionTableEntry.decode(reader);

        // TODO: Fix this awful, dangerous, slow hack
        var attributes_length = try reader.readIntBig(u16);
        var attributes_index: usize = 0;
        var attributes = std.ArrayList(AttributeInfo).init(allocator);
        while (attributes_index < attributes_length) : (attributes_index += 1) {
            var decoded = try AttributeInfo.decode(constant_pool, allocator, reader);
            if (decoded == .unknown) {
                attributes_length -= 1;
                continue;
            }
            try attributes.append(decoded);
        }

        return CodeAttribute{
            .constant_pool = constant_pool,

            .max_stack = max_stack,
            .max_locals = max_locals,

            .code = code,
            .exception_table = exception_table,

            .attributes = attributes,
        };
    }

    pub fn calcAttrLen(self: CodeAttribute) u32 {
        var len: u32 = 2 * 4 + 4 + @intCast(u32, self.code.items.len);
        for (self.attributes.items) |att| len += att.calcAttrLen();
        for (self.exception_table.items) |att| len += att.calcAttrLen();
        return len;
    }

    pub fn encode(self: CodeAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, self.max_stack);
        try writer.writeIntBig(u16, self.max_locals);

        try writer.writeIntBig(u32, @intCast(u32, self.code.items.len));
        try writer.writeAll(self.code.items);

        try writer.writeIntBig(u16, @intCast(u16, self.exception_table.items.len));
        for (self.exception_table.items) |et| try et.encode(writer);

        try writer.writeIntBig(u16, @intCast(u16, self.attributes.items.len));
        for (self.attributes.items) |at| try at.encode(writer);
    }

    pub fn deinit(self: CodeAttribute) void {
        self.code.deinit();
        self.attributes.deinit();
    }
};
