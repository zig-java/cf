const std = @import("std");
const utils = @import("utils.zig");
const AttributeInfo = @import("attributes.zig").AttributeInfo;
const ClassFile = @import("ClassFile.zig");
const ConstantPool = @import("ConstantPool.zig");

const FieldInfo = @This();

pub const AccessFlags = struct {
    public: bool = false,
    private: bool = false,
    protected: bool = false,
    static: bool = false,
    final: bool = false,
    @"volatile": bool = false,
    transient: bool = false,
    synthetic: bool = false,
    enum_member: bool = false,
};

constant_pool: *const ConstantPool,

access_flags: AccessFlags,
name_index: u16,
descriptor_index: u16,
attributes: std.ArrayList(AttributeInfo),

pub fn getName(self: FieldInfo) ConstantPool.Utf8Info {
    return self.constant_pool.get(self.name_index).utf8;
}

pub fn getDescriptor(self: FieldInfo) ConstantPool.Utf8Info {
    return self.constant_pool.get(self.descriptor_index).utf8;
}

pub fn decode(constant_pool: *const ConstantPool, allocator: *std.mem.Allocator, reader: anytype) !FieldInfo {
    var access_flags_u = try reader.readIntBig(u16);
    var name_index = try reader.readIntBig(u16);
    var descriptor_index = try reader.readIntBig(u16);

    // var att_count = try reader.readIntBig(u16);
    // var att = try std.ArrayList(AttributeInfo).initCapacity(allocator, att_count);
    // for (att.items) |*a| a.* = try AttributeInfo.decode(constant_pool, allocator, reader);
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

    return FieldInfo{
        .constant_pool = constant_pool,

        .access_flags = .{
            .public = utils.isPresent(u16, access_flags_u, 0x0001),
            .private = utils.isPresent(u16, access_flags_u, 0x0002),
            .protected = utils.isPresent(u16, access_flags_u, 0x0004),
            .static = utils.isPresent(u16, access_flags_u, 0x0008),
            .final = utils.isPresent(u16, access_flags_u, 0x0010),
            .@"volatile" = utils.isPresent(u16, access_flags_u, 0x0040),
            .transient = utils.isPresent(u16, access_flags_u, 0x0080),
            .synthetic = utils.isPresent(u16, access_flags_u, 0x1000),
            .enum_member = utils.isPresent(u16, access_flags_u, 0x4000),
        },
        .name_index = name_index,
        .descriptor_index = descriptor_index,
        .attributes = attributes,
    };
}

pub fn encode(self: FieldInfo, writer: anytype) !void {
    var access_flags_u: u16 = 0;
    if (self.access_flags.public) utils.setPresent(u16, &access_flags_u, 0x0001);
    if (self.access_flags.private) utils.setPresent(u16, &access_flags_u, 0x0002);
    if (self.access_flags.protected) utils.setPresent(u16, &access_flags_u, 0x0004);
    if (self.access_flags.static) utils.setPresent(u16, &access_flags_u, 0x0008);
    if (self.access_flags.final) utils.setPresent(u16, &access_flags_u, 0x0010);
    if (self.access_flags.@"volatile") utils.setPresent(u16, &access_flags_u, 0x0040);
    if (self.access_flags.transient) utils.setPresent(u16, &access_flags_u, 0x0080);
    if (self.access_flags.synthetic) utils.setPresent(u16, &access_flags_u, 0x1000);
    if (self.access_flags.enum_member) utils.setPresent(u16, &access_flags_u, 0x4000);
    try writer.writeIntBig(u16, access_flags_u);

    try writer.writeIntBig(u16, self.name_index);
    try writer.writeIntBig(u16, self.descriptor_index);

    try writer.writeIntBig(u16, @intCast(u16, self.attributes.items.len));
    for (self.attributes.items) |att| try att.encode(writer);
}
