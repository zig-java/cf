const std = @import("std");
const ConstantPool = @import("ConstantPool.zig");

const logger = std.log.scoped(.cf_attributes);

// TODO: Implement all attribute types
pub const AttributeInfo = union(enum) {
    constant_value: ConstantValueAttribute,
    runtime_visible_annotations: RuntimeVisibleAnnotationsAttribute,
    deprecated: DeprecatedAttribute,
    code: CodeAttribute,
    line_number_table: LineNumberTableAttribute,
    source_file: SourceFileAttribute,
    exceptions: ExceptionsAttribute,
    unknown: void,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) anyerror!AttributeInfo {
        var attribute_name_index = try reader.readIntBig(u16);
        var attribute_length = try reader.readIntBig(u32);

        var info = try allocator.alloc(u8, attribute_length);
        defer allocator.free(info);
        _ = try reader.readAll(info);

        var fbs = std.io.fixedBufferStream(info);
        var name = constant_pool.get(attribute_name_index).utf8.bytes;

        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.type == void) {} else {
                if (std.mem.eql(u8, @field(field.type, "name"), name)) {
                    return @unionInit(AttributeInfo, field.name, try @field(field.type, "decode")(constant_pool, allocator, fbs.reader()));
                }
            }
        }

        logger.err("Could not decode attribute: {s}", .{name});
        return .unknown;
    }

    pub fn calcAttrLen(self: AttributeInfo) u32 {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.type == void) continue;

            if (std.meta.activeTag(self) == @field(std.meta.Tag(AttributeInfo), field.name)) {
                return @field(self, field.name).calcAttrLen() + 6; // 6 intro bytes!
            }
        }

        unreachable;
    }

    pub fn deinit(self: *AttributeInfo) void {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.type == void) continue;

            if (std.meta.activeTag(self.*) == @field(std.meta.Tag(AttributeInfo), field.name)) {
                @field(self, field.name).deinit();
            }
        }
    }

    pub fn encode(self: AttributeInfo, writer: anytype) !void {
        inline for (std.meta.fields(AttributeInfo)) |field| {
            if (field.type == void) continue;

            if (std.meta.activeTag(self) == @field(std.meta.Tag(AttributeInfo), field.name)) {
                var attr = @field(self, field.name);

                try writer.writeIntBig(u16, try attr.constant_pool.locateUtf8Entry(@field(field.type, "name")));
                try writer.writeIntBig(u32, attr.calcAttrLen());

                try attr.encode(writer);
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

    pub fn encode(self: ExceptionTableEntry, writer: anytype) !void {
        try writer.writeIntBig(u16, self.start_pc);
        try writer.writeIntBig(u16, self.end_pc);
        try writer.writeIntBig(u16, self.handler_pc);
        try writer.writeIntBig(u16, self.catch_type);
    }
};

pub const CodeAttribute = struct {
    pub const name = "Code";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    max_stack: u16,
    max_locals: u16,

    code: std.ArrayListUnmanaged(u8),
    exception_table: std.ArrayListUnmanaged(ExceptionTableEntry),

    attributes: std.ArrayListUnmanaged(AttributeInfo),

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !CodeAttribute {
        var max_stack = try reader.readIntBig(u16);
        var max_locals = try reader.readIntBig(u16);

        var code_length = try reader.readIntBig(u32);
        var code = try std.ArrayListUnmanaged(u8).initCapacity(allocator, code_length);
        code.items.len = code_length;
        _ = try reader.readAll(code.items);

        var exception_table_len = try reader.readIntBig(u16);
        var exception_table = try std.ArrayListUnmanaged(ExceptionTableEntry).initCapacity(allocator, exception_table_len);
        exception_table.items.len = exception_table_len;
        for (exception_table.items) |*et| et.* = try ExceptionTableEntry.decode(reader);

        var attributes_length = try reader.readIntBig(u16);
        var attributes_index: usize = 0;
        var attributes = try std.ArrayListUnmanaged(AttributeInfo).initCapacity(allocator, attributes_length);
        while (attributes_index < attributes_length) : (attributes_index += 1) {
            var decoded = try AttributeInfo.decode(constant_pool, allocator, reader);
            if (decoded == .unknown) {
                attributes_length -= 1;
                continue;
            }
            try attributes.append(allocator, decoded);
        }

        return CodeAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,

            .max_stack = max_stack,
            .max_locals = max_locals,

            .code = code,
            .exception_table = exception_table,

            .attributes = attributes,
        };
    }

    pub fn calcAttrLen(self: CodeAttribute) u32 {
        var len: u32 = 2 + 2 + 4 + @intCast(u32, self.code.items.len) + 2 + 2;
        for (self.attributes.items) |att| len += att.calcAttrLen();
        len += 8 * @intCast(u32, self.exception_table.items.len);
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

    pub fn deinit(self: *CodeAttribute) void {
        self.code.deinit(self.allocator);
        self.exception_table.deinit(self.allocator);
        for (self.attributes.items) |*attr| {
            attr.deinit();
        }
        self.attributes.deinit(self.allocator);
    }
};

pub const LineNumberTableEntry = struct {
    /// The index into the code array at which the code for a new line in the original source file begins
    start_pc: u16,
    /// The corresponding line number in the original source file
    line_number: u16,

    pub fn decode(reader: anytype) !LineNumberTableEntry {
        var entry: LineNumberTableEntry = undefined;
        entry.start_pc = try reader.readIntBig(u16);
        entry.line_number = try reader.readIntBig(u16);
        return entry;
    }

    pub fn encode(self: LineNumberTableEntry, writer: anytype) !void {
        try writer.writeIntBig(u16, self.start_pc);
        try writer.writeIntBig(u16, self.line_number);
    }
};

pub const LineNumberTableAttribute = struct {
    pub const name = "LineNumberTable";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    line_number_table: std.ArrayListUnmanaged(LineNumberTableEntry),

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !LineNumberTableAttribute {
        var line_number_table_length = try reader.readIntBig(u16);

        var line_number_table = try std.ArrayListUnmanaged(LineNumberTableEntry).initCapacity(allocator, line_number_table_length);
        line_number_table.items.len = line_number_table_length;
        for (line_number_table.items) |*entry| entry.* = try LineNumberTableEntry.decode(reader);

        return LineNumberTableAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,

            .line_number_table = line_number_table,
        };
    }

    pub fn calcAttrLen(self: LineNumberTableAttribute) u32 {
        var len: u32 = 2 + 4 * @intCast(u32, self.line_number_table.items.len);
        return len;
    }

    pub fn encode(self: LineNumberTableAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, @intCast(u16, self.line_number_table.items.len));
        for (self.line_number_table.items) |entry| try entry.encode(writer);
    }

    pub fn deinit(self: *LineNumberTableAttribute) void {
        self.line_number_table.deinit(self.allocator);
    }
};

pub const SourceFileAttribute = struct {
    pub const name = "SourceFile";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    source_file_index: u16,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !SourceFileAttribute {
        return SourceFileAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,

            .source_file_index = try reader.readIntBig(u16),
        };
    }

    pub fn calcAttrLen(self: SourceFileAttribute) u32 {
        _ = self;
        return 2;
    }

    pub fn encode(self: SourceFileAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, self.source_file_index);
    }

    pub fn deinit(self: *SourceFileAttribute) void {
        _ = self;
    }
};

pub const ExceptionsAttribute = struct {
    pub const name = "Exceptions";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    exception_index_table: std.ArrayListUnmanaged(u16),

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !ExceptionsAttribute {
        var exception_index_table_length = try reader.readIntBig(u16);

        var exception_index_table = try std.ArrayListUnmanaged(u16).initCapacity(allocator, exception_index_table_length);
        exception_index_table.items.len = exception_index_table_length;
        for (exception_index_table.items) |*entry| entry.* = try reader.readIntBig(u16);

        return ExceptionsAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,

            .exception_index_table = exception_index_table,
        };
    }

    pub fn calcAttrLen(self: ExceptionsAttribute) u32 {
        var len: u32 = 2 + 2 * @intCast(u32, self.exception_index_table.items.len);
        return len;
    }

    pub fn encode(self: ExceptionsAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, @intCast(u16, self.exception_index_table.items.len));
        for (self.exception_index_table.items) |entry| try writer.writeIntBig(u16, entry);
    }

    pub fn deinit(self: *ExceptionsAttribute) void {
        self.exception_index_table.deinit(self.allocator);
    }
};

pub const ConstantValueAttribute = struct {
    pub const name = "ConstantValue";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    constantvalue_index: u16,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !ConstantValueAttribute {
        return ConstantValueAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,

            .constantvalue_index = try reader.readIntBig(u16),
        };
    }

    pub fn calcAttrLen(self: ConstantValueAttribute) u32 {
        _ = self;
        return 2;
    }

    pub fn encode(self: ConstantValueAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, self.constantvalue_index);
    }

    pub fn deinit(self: *ConstantValueAttribute) void {
        _ = self;
    }
};

pub const DeprecatedAttribute = struct {
    pub const name = "Deprecated";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !DeprecatedAttribute {
        _ = reader;
        return DeprecatedAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,
        };
    }

    pub fn calcAttrLen(self: DeprecatedAttribute) u32 {
        _ = self;
        return 0;
    }

    pub fn encode(self: DeprecatedAttribute, writer: anytype) anyerror!void {
        _ = self;
        _ = writer;
    }

    pub fn deinit(self: *DeprecatedAttribute) void {
        _ = self;
    }
};

// Annotations
pub const ElementTag = enum(u8) {
    // Primitive types
    Byte = 'B',
    Char = 'C',
    Double = 'D',
    Float = 'F',
    Int = 'I',
    Long = 'J',
    Short = 'S',
    Boolean = 'Z',

    // Other legal values
    String = 's',
    EnumConstant = 'e',
    Class = 'c',
    AnnotationType = '@',
    Array = '[',
};

pub const ElementValue = union(ElementTag) {
    Byte: u16,
    Char: u16,
    Double: u16,
    Float: u16,
    Int: u16,
    Long: u16,
    Short: u16,
    Boolean: u16,
    String: u16,
    EnumConstant: struct {
        type_name_index: u16,
        const_name_index: u16,
    },
    Class: u16,
    AnnotationType: *Annotation,
    Array: struct {
        num_values: u16,
        values: []ElementValue,
    },

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) anyerror!ElementValue {
        const value_tag = try reader.readEnum(ElementTag, .Big);
        const value: ElementValue = switch (value_tag) {
            .Byte => .{ .Byte = try reader.readIntBig(u16) },
            .Char => .{ .Char = try reader.readIntBig(u16) },
            .Double => .{ .Double = try reader.readIntBig(u16) },
            .Float => .{ .Float = try reader.readIntBig(u16) },
            .Int => .{ .Int = try reader.readIntBig(u16) },
            .Long => .{ .Long = try reader.readIntBig(u16) },
            .Short => .{ .Short = try reader.readIntBig(u16) },
            .Boolean => .{ .Boolean = try reader.readIntBig(u16) },
            .String => .{ .String = try reader.readIntBig(u16) },
            .EnumConstant => .{ .EnumConstant = .{
                .type_name_index = try reader.readIntBig(u16),
                .const_name_index = try reader.readIntBig(u16),
            } },
            .Class => .{ .Class = try reader.readIntBig(u16) },
            .AnnotationType => annotation: {
                const annotation = try allocator.create(Annotation);
                annotation.* = try Annotation.decode(allocator, reader);
                break :annotation .{ .AnnotationType = annotation };
            },
            .Array => array: {
                const num_values = try reader.readIntBig(u16);
                const values = try allocator.alloc(ElementValue, num_values);
                for (values) |*value| {
                    value.* = try ElementValue.decode(allocator, reader);
                }
                break :array .{ .Array = .{
                    .num_values = num_values,
                    .values = values,
                } };
            },
        };
        return value;
    }

    pub fn encode(self: ElementValue, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, @enumToInt(self));
        switch (self) {
            .EnumConstant => |econst| {
                try writer.writeIntBig(u16, econst.type_name_index);
                try writer.writeIntBig(u16, econst.const_name_index);
            },
            .AnnotationType => |anno| {
                try anno.encode(writer);
            },
            .Array => |array| {
                try writer.writeIntBig(u16, array.num_values);
                for (array.values) |value| try value.encode(writer);
            },
            inline else => |value| try writer.writeIntBig(u16, value),
        }
    }

    pub fn calcAttrLen(self: ElementValue) u32 {
        return switch (self) {
            .EnumConstant => 4,
            .AnnotationType => |anno| anno.calcAttrLen(),
            .Array => |array| 2 + array.num_values * 2,
            else => 2,
        };
    }

    pub fn deinit(self: ElementValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .AnnotationType => |anno| anno.deinit(allocator),
            .Array => |array| allocator.free(array.values),
            else => {},
        }
    }
};

pub const AnnotationValuePair = struct {
    element_name_index: u16,
    value: ElementValue,
    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !AnnotationValuePair {
        const element_name_index = try reader.readIntBig(u16);
        return AnnotationValuePair{
            .element_name_index = element_name_index,
            .value = try ElementValue.decode(allocator, reader),
        };
    }

    pub fn encode(self: AnnotationValuePair, writer: anytype) !void {
        try writer.writeIntBig(u16, self.element_name_index);
        try self.value.encode(writer);
    }

    pub fn calcAttrLen(self: AnnotationValuePair) u32 {
        return 2 + self.value.calcAttrLen();
    }

    pub fn deinit(self: AnnotationValuePair, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
    }
};

pub const Annotation = struct {
    type_index: u16,
    num_element_value_pairs: u16,
    element_value_pairs: []AnnotationValuePair,
    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Annotation {
        const type_index = try reader.readIntBig(u16);
        const num_element_value_pairs = try reader.readIntBig(u16);
        const element_value_pairs = try allocator.alloc(AnnotationValuePair, num_element_value_pairs);
        for (element_value_pairs) |*value_pair| {
            value_pair.* = try AnnotationValuePair.decode(allocator, reader);
        }

        return Annotation{
            .type_index = type_index,
            .num_element_value_pairs = num_element_value_pairs,
            .element_value_pairs = element_value_pairs,
        };
    }

    pub fn encode(self: Annotation, writer: anytype) !void {
        try writer.writeIntBig(u16, self.type_index);
        try writer.writeIntBig(u16, self.num_element_value_pairs);
        for (self.element_value_pairs) |pair| try pair.encode(writer);
    }

    pub fn calcAttrLen(self: Annotation) u32 {
        var len: u32 = 4;
        for (self.element_value_pairs) |pair| len += pair.calcAttrLen();
        return len;
    }

    pub fn deinit(self: Annotation, allocator: std.mem.Allocator) void {
        for (self.element_value_pairs) |value_pair| {
            value_pair.deinit(allocator);
        }
        allocator.free(self.element_value_pairs);
    }
};

pub const RuntimeVisibleAnnotationsAttribute = struct {
    pub const name = "RuntimeVisibleAnnotations";

    allocator: std.mem.Allocator,
    constant_pool: *ConstantPool,

    num_annotations: u16,
    annotations: []Annotation,

    pub fn decode(constant_pool: *ConstantPool, allocator: std.mem.Allocator, reader: anytype) !RuntimeVisibleAnnotationsAttribute {
        const num_annotations = try reader.readIntBig(u16);
        const annotations = try allocator.alloc(Annotation, num_annotations);
        for (annotations) |*annotation| {
            annotation.* = try Annotation.decode(allocator, reader);
        }

        return RuntimeVisibleAnnotationsAttribute{
            .allocator = allocator,
            .constant_pool = constant_pool,
            .num_annotations = num_annotations,
            .annotations = annotations,
        };
    }

    pub fn calcAttrLen(self: RuntimeVisibleAnnotationsAttribute) u32 {
        var len: u32 = 2;
        for (self.annotations) |anno| len += anno.calcAttrLen();
        return len;
    }

    pub fn encode(self: RuntimeVisibleAnnotationsAttribute, writer: anytype) anyerror!void {
        try writer.writeIntBig(u16, self.num_annotations);
        for (self.annotations) |annotation| try annotation.encode(writer);
    }

    pub fn deinit(self: *RuntimeVisibleAnnotationsAttribute) void {
        for (self.annotations) |annotation| annotation.deinit(self.allocator);
        self.allocator.free(self.annotations);
    }
};
