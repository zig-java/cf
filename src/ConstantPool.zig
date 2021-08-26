const std = @import("std");
const descriptors = @import("descriptors.zig");

const ConstantPool = @This();

entries: std.ArrayList(Entry),

pub fn init(allocator: *std.mem.Allocator) ConstantPool {
    return ConstantPool{ .entries = std.ArrayList(Entry).init(allocator) };
}

pub fn get(self: ConstantPool, index: u16) Entry {
    return self.entries[index - 1];
}

fn Serialize(comptime T: type) type {
    return struct {
        pub fn decode(constant_pool: *const ConstantPool, reader: anytype) !T {
            var value: T = undefined;
            value.constant_pool = constant_pool;

            inline for (std.meta.fields(T)[1..]) |field| {
                @field(value, field.name) = switch (@typeInfo(field.field_type)) {
                    .Int => try reader.readIntBig(field.field_type),
                    .Enum => |info| @intToEnum(field.field_type, info.tag_type),
                    else => @compileError("Decode not implemented: " ++ @typeName(field.field_type)),
                };
            }
        }
    };
}

pub fn decodeEntry(allocator: *std.mem.Allocator, reader: anytype) !Entry {
    var tag = try reader.readIntBig(u8);
    inline for (@typeInfo(Tag).Enum.fields) |f| {
        const this_tag_value = @field(Tag, f.name);
        if (tag == @enumToInt(this_tag_value)) {
            var value = if (@hasDecl(f.field_type, "decode")) try @field(f.field_type, "decode")(self, allocator, reader) else try Serialize(f.field_type).decode(self, reader);
            return @unionInit(Entry, f.name, value);
        }
    }

    unreachable;
}

pub const Tag = enum(u8) {
    class = 7,
    fieldref = 9,
    methodref = 10,
    interface_methodref = 11,
    string = 8,
    integer = 3,
    float = 4,
    long = 5,
    double = 6,
    name_and_type = 12,
    utf8 = 1,
    method_handle = 15,
    method_type = 16,
    dynamic = 17,
    invoke_dynamic = 18,
    module = 19,
    package = 20,
};

pub const ClassInfo = struct {
    constant_pool: *const ConstantPool,

    /// Points to a `Utf8Info`
    name_index: u16,

    pub fn getName(self: ClassInfo) Utf8Info {
        return self.constant_pool.get(self.name_index).utf8;
    }
};

pub const RefInfo = struct {
    constant_pool: *const ConstantPool,

    /// Points to class or interface
    class_index: u16,
    /// Points to a `NameAndTypeInfo`
    name_and_type_index: u16,

    pub fn getClassInfo(self: RefInfo) ClassInfo {
        return self.constant_pool.get(self.class_index).class;
    }

    pub fn getNameAndTypeInfo(self: RefInfo) NameAndTypeInfo {
        return self.constant_pool.get(self.name_and_type_index).name_and_type;
    }
};

/// Points to a `Utf8Info`
pub const StringInfo = struct {
    constant_pool: *const ConstantPool,

    string_index: u16,
};

/// Represents 4-byte (32 bit) integer
pub const IntegerInfo = struct {
    constant_pool: *const ConstantPool,

    bytes: u32,
};

/// Represents 4-byte (32 bit) float
pub const FloatInfo = struct {
    constant_pool: *const ConstantPool,

    value: u32,
};

pub const LongInfo = struct {
    constant_pool: *const ConstantPool,

    value: u64,
};

pub const DoubleInfo = struct {
    constant_pool: *const ConstantPool,

    value: u64,
};

pub const NameAndTypeInfo = struct {
    const Self = @This();

    constant_pool: *const ConstantPool,

    /// Points to a `Utf8Info` describing a unique field or method name or <init>
    name_index: u16,
    /// Points to a `Utf8Info` representing a field or method descriptor
    descriptor_index: u16,

    pub fn getName(self: Self, constant_pool: []Entry) Utf8Info {
        return constant_pool[self.name_index - 1].utf8;
    }

    pub fn getDescriptor(self: Self, constant_pool: []Entry) Utf8Info {
        return constant_pool[self.descriptor_index - 1].utf8;
    }
};

pub const Utf8Info = struct {
    const Self = @This();

    constant_pool: *const ConstantPool,

    bytes: []u8,

    fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype) !Self {
        var length = try reader.readIntBig(u16);
        var bytes = try allocator.alloc(u8, length);
        _ = try reader.readAll(bytes);

        return Self{
            .constant_pool = constant_pool,
            .bytes = bytes,
        };
    }
};

pub const ReferenceKind = enum(u8) { get_field = 1, get_static = 2, put_field = 3, put_static = 4, invoke_virtual = 5, invoke_static = 6, invoke_special = 7, new_invoke_special = 8, invoke_interface = 9 };

pub const MethodHandleInfo = struct {
    const Self = @This();

    constant_pool: *const ConstantPool,

    reference_kind: ReferenceKind,
    /// Based on ref kind:
    /// 1, 2, 3, 4 - points to fieldref
    /// 5, 8 - points to methodref
    /// 6, 7 - points to methodref or interfacemethodref
    /// 9 - Must point to interfacemethodref
    reference_index: u16,

    // fn parse(allocator: *std.mem.Allocator, reader: anytype) !Self {
    //     return Self{
    //         .reference_kind = @intToEnum(ReferenceKind, try reader.readIntBig(u8)),
    //         .reference_index = try reader.readIntBig(u16),
    //     };
    // }

    pub fn getReference(self: Self, constant_pool: []Entry) Info {
        var ref = constant_pool[self.reference_index - 1];
        switch (self.reference_kind) {
            .get_field, .get_static, .put_field, .put_static => std.debug.assert(std.meta.activeTag(ref) == .fieldref),

            .invoke_virtual, .new_invoke_special => std.debug.assert(std.meta.activeTag(ref) == .methodref),

            .invoke_static, .invoke_special => std.debug.assert(std.meta.activeTag(ref) == .methodref or std.meta.activeTag(ref) == .interface_methodref),

            .invoke_interface => std.debug.assert(std.meta.activeTag(ref) == .interface_methodref),
        }
        return ref;
    }
};

pub const MethodTypeInfo = struct {
    const Self = @This();

    constant_pool: *const ConstantPool,

    descriptor_index: u16,

    pub fn getDescriptor(self: Self, constant_pool: []Entry) Utf8Info {
        return constant_pool[self.descriptor_index - 1].utf8;
    }
};

pub const DynamicInfo = struct {
    constant_pool: *const ConstantPool,

    bootstrap_method_attr_index: u16,
    name_and_type_index: u16,

    pub fn getNameAndTypeInfo(self: Self, constant_pool: []Entry) NameAndTypeInfo {
        return constant_pool[self.name_and_type_index - 1].name_and_type;
    }
};

pub const InvokeDynamicInfo = struct {
    constant_pool: *const ConstantPool,

    bootstrap_method_attr_index: u16,
    name_and_type_index: u16,

    pub fn getNameAndTypeInfo(self: Self, constant_pool: []Entry) NameAndTypeInfo {
        return constant_pool[self.name_and_type_index - 1].name_and_type;
    }
};

pub const ModuleInfo = struct {
    constant_pool: *const ConstantPool,

    name_index: u16,

    pub fn getName(self: Self, constant_pool: []Entry) Utf8Info {
        return constant_pool[self.name_index - 1].utf8;
    }
};

pub const PackageInfo = struct {
    constant_pool: *const ConstantPool,

    name_index: u16,

    pub fn getName(self: Self, constant_pool: []Entry) Utf8Info {
        return constant_pool[self.name_index - 1].utf8;
    }
};

pub const Entry = union(Tag) {
    const Self = @This();

    class: ClassInfo,

    fieldref: RefInfo,
    methodref: RefInfo,
    interface_methodref: RefInfo,

    string: StringInfo,
    integer: IntegerInfo,
    float: FloatInfo,
    long: LongInfo,
    double: DoubleInfo,

    name_and_type: NameAndTypeInfo,
    utf8: Utf8Info,

    method_handle: MethodHandleInfo,
    method_type: MethodTypeInfo,

    dynamic: DynamicInfo,
    invoke_dynamic: InvokeDynamicInfo,

    module: ModuleInfo,
    package: PackageInfo,
};
