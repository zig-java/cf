const std = @import("std");
const utils = @import("utils.zig");
const attributes = @import("attributes.zig");
const ConstantPool = @import("ConstantPool.zig");

const MethodInfo = @This();

pub const AccessFlags = struct {
    public: bool = false,
    private: bool = false,
    protected: bool = false,
    static: bool = false,
    final: bool = false,
    synchronized: bool = false,
    bridge: bool = false,
    varargs: bool = false,
    native: bool = false,
    abstract: bool = false,
    strict: bool = false,
    synthetic: bool = false,
};

constant_pool: *const ConstantPool,

access_flags: AccessFlags,
name_index: u16,
descriptor_index: u16,
attributes: []attributes.AttributeInfo,

pub fn getName(self: MethodInfo) ConstantPool.Utf8Info {
    return self.constant_pool.get(self.name_index).utf8;
}

pub fn getDescriptor(self: MethodInfo) ConstantPool.Utf8Info {
    return self.constant_pool.get(self.descriptor_index).utf8;
}

pub fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype) !MethodInfo {
    var access_flags_u = try reader.readIntBig(u16);
    var name_index = try reader.readIntBig(u16);
    var descriptor_index = try reader.readIntBig(u16);

    var att_count = try reader.readIntBig(u16);
    var att = try allocator.alloc(attributes.AttributeInfo, att_count);
    for (att) |*a| a.* = try attributes.AttributeInfo.decode(constant_pool, allocator, reader);

    return MethodInfo{
        .constant_pool = constant_pool,

        .access_flags = .{
            .public = utils.isPresent(u16, access_flags_u, 0x0001),
            .private = utils.isPresent(u16, access_flags_u, 0x0002),
            .protected = utils.isPresent(u16, access_flags_u, 0x0004),
            .static = utils.isPresent(u16, access_flags_u, 0x0008),
            .final = utils.isPresent(u16, access_flags_u, 0x0010),
            .synchronized = utils.isPresent(u16, access_flags_u, 0x0020),
            .bridge = utils.isPresent(u16, access_flags_u, 0x0040),
            .varargs = utils.isPresent(u16, access_flags_u, 0x0080),
            .native = utils.isPresent(u16, access_flags_u, 0x0100),
            .abstract = utils.isPresent(u16, access_flags_u, 0x0400),
            .strict = utils.isPresent(u16, access_flags_u, 0x0800),
            .synthetic = utils.isPresent(u16, access_flags_u, 0x1000),
        },
        .name_index = name_index,
        .descriptor_index = descriptor_index,
        .attributes = att,
    };
}
