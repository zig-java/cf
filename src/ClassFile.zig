//! A Java ClassFile parser matching the spec detailed here: https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1
//! It's important to note that "class" file is misleading as classes as well as interfaces and enums are described here
// NOTE: The JDK docs refer to numeric types as u1, u2, etc. - these are in BYTES, not BITS (u1 = u8, u2 = u16, etc.)

const std = @import("std");
const utils = @import("utils.zig");
const attributes = @import("attributes.zig");
const FieldInfo = @import("FieldInfo.zig");
const MethodInfo = @import("MethodInfo.zig");
const ConstantPool = @import("ConstantPool.zig");

const ClassFile = @This();

/// Denotes access permissions to and properties of this class or interface
pub const AccessFlags = struct {
    /// Declared public; may be accessed from outside its package
    public: bool = false,
    /// Declared final; no subclasses allowed.
    final: bool = false,
    /// Treat superclass methods specially when invoked by the invokespecial instruction
    super: bool = false,
    /// Is an interface, not a class
    interface: bool = false,
    /// Declared abstract; must not be instantiated
    abstract: bool = false,
    /// Declared synthetic; not present in the source code
    synthetic: bool = false,
    /// Declared as an annotation interface
    annotation: bool = false,
    /// Declared as an enum class
    enum_class: bool = false,
    /// Is a module, not a class or interface
    module: bool = false,
};

// To see what the major and minor versions actually correspond to, see https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1-200-B.2
minor_version: u16,
major_version: u16,
/// The constant_pool is a table of structures ([§4.4](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4)) representing various string constants, class and interface names, field names, and other constants that are referred to within the ClassFile structure and its substructures
///
/// The constant_pool table is indexed from 1 to ConstantPoolcount - 1
constant_pool: *ConstantPool,
/// The value of the access_flags item is a mask of flags used to denote access permissions to and properties of this class or interface. The interpretation of each flag, when set, is specified in [Table 4.1-B](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1-200-E.1)
access_flags: AccessFlags,
/// The value of the this_class item must be a valid index into the constant_pool table and the entry at that index must be a CONSTANT_Class_info structure ([§4.4.1](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.4.1)) representing the class or interface defined by this class file
this_class: u16,
/// For a class, the value of the super_class item either must be zero or must be a valid index into the constant_pool table. If the value of the super_class item is nonzero, the constant_pool entry at that index must be a CONSTANT_Class_info structure representing the direct superclass of the class defined by this class file. Neither the direct superclass nor any of its superclasses may have the ACC_FINAL flag set in the access_flags item of its ClassFile structure.
///
/// If the value of the super_class item is zero, then this class file must represent the class Object, the only class or interface without a direct superclass.
///
/// For an interface, the value of the super_class item must always be a valid index into the constant_pool table. The constant_pool entry at that index must be a CONSTANT_Class_info structure representing the class Object.
super_class: ?u16,
/// Each value in the interfaces array must be a valid index into the constant_pool table and the entry at each value of interfaces[i], where 0 ≤ i < interfaces_count, must be a CONSTANT_Class_info structure representing an interface that is a direct superinterface of this class or interface type, in the left-to-right order given in the source for the type
interfaces: std.ArrayList(u16),
/// Fields this class has
fields: std.ArrayList(FieldInfo),
/// Methods the class has
methods: std.ArrayList(MethodInfo),
/// Attributes the class has
attributes: std.ArrayList(attributes.AttributeInfo),

pub fn getConstantPoolEntry(self: ClassFile, index: u16) ConstantPool.Entry {
    return self.constant_pool[index - 1];
}

pub fn decode(allocator: *std.mem.Allocator, reader: anytype) !ClassFile {
    var magic = try reader.readIntBig(u32);
    if (magic != 0xCAFEBABE) return error.BadMagicValue;

    var minor_version = try reader.readIntBig(u16);
    var major_version = try reader.readIntBig(u16);

    var constant_pool = try ConstantPool.init(allocator);
    var z = (try reader.readIntBig(u16)) - 1;
    try constant_pool.entries.ensureTotalCapacity(z);
    constant_pool.entries.items.len = z;
    try constant_pool.decodeEntries(allocator, reader);

    var access_flags_u = try reader.readIntBig(u16);
    var access_flags = AccessFlags{
        .public = utils.isPresent(u16, access_flags_u, 0x0001),
        .final = utils.isPresent(u16, access_flags_u, 0x0010),
        .super = utils.isPresent(u16, access_flags_u, 0x0020),
        .interface = utils.isPresent(u16, access_flags_u, 0x0200),
        .abstract = utils.isPresent(u16, access_flags_u, 0x0400),
        .synthetic = utils.isPresent(u16, access_flags_u, 0x1000),
        .annotation = utils.isPresent(u16, access_flags_u, 0x2000),
        .enum_class = utils.isPresent(u16, access_flags_u, 0x4000),
        .module = utils.isPresent(u16, access_flags_u, 0x8000),
    };

    var this_class_u = try reader.readIntBig(u16);
    var this_class = this_class_u;

    var super_class_u = try reader.readIntBig(u16);
    var super_class = if (super_class_u == 0) null else super_class_u;

    var interface_count = try reader.readIntBig(u16);
    var interfaces = try std.ArrayList(u16).initCapacity(allocator, interface_count);
    interfaces.items.len = interface_count;
    for (interfaces.items) |*i| i.* = try reader.readIntBig(u16);

    var field_count = try reader.readIntBig(u16);
    var fieldss = try std.ArrayList(FieldInfo).initCapacity(allocator, field_count);
    fieldss.items.len = field_count;
    for (fieldss.items) |*f| f.* = try FieldInfo.decode(constant_pool, allocator, reader);

    var method_count = try reader.readIntBig(u16);
    var methodss = try std.ArrayList(MethodInfo).initCapacity(allocator, method_count);
    methodss.items.len = method_count;
    for (methodss.items) |*m| m.* = try MethodInfo.decode(constant_pool, allocator, reader);

    // var attributess = try std.ArrayList(attributes.AttributeInfo).initCapacity(allocator, try reader.readIntBig(u16));
    // for (attributess.items) |*a| a.* = try attributes.AttributeInfo.decode(&constant_pool, allocator, reader);
    // TODO: Fix this awful, dangerous, slow hack
    var attributes_length = try reader.readIntBig(u16);
    var attributes_index: usize = 0;
    var attributess = std.ArrayList(attributes.AttributeInfo).init(allocator);
    while (attributes_index < attributes_length) : (attributes_index += 1) {
        var decoded = try attributes.AttributeInfo.decode(constant_pool, allocator, reader);
        if (decoded == .unknown) {
            attributes_length -= 1;
            continue;
        }
        try attributess.append(decoded);
    }

    return ClassFile{
        .minor_version = minor_version,
        .major_version = major_version,
        .constant_pool = constant_pool,
        .access_flags = access_flags,
        .this_class = this_class,
        .super_class = super_class,
        .interfaces = interfaces,
        .fields = fieldss,
        .methods = methodss,
        .attributes = attributess,
    };
}

pub fn encode(self: *const ClassFile, writer: anytype) !void {
    try writer.writeIntBig(u32, 0xCAFEBABE);

    try writer.writeIntBig(u16, self.minor_version);
    try writer.writeIntBig(u16, self.major_version);
    try writer.writeIntBig(u16, @intCast(u16, self.constant_pool.entries.items.len) + 1);

    var constant_pool_index: usize = 0;
    while (constant_pool_index < self.constant_pool.entries.items.len) : (constant_pool_index += 1) {
        var cp = self.constant_pool.entries.items[constant_pool_index];
        try cp.encode(writer);

        if (cp == .double or cp == .long) {
            constant_pool_index += 1;
        }
    }

    var access_flags_u: u16 = 0;
    if (self.access_flags.public) utils.setPresent(u16, &access_flags_u, 0x0001);
    if (self.access_flags.final) utils.setPresent(u16, &access_flags_u, 0x0010);
    if (self.access_flags.super) utils.setPresent(u16, &access_flags_u, 0x0020);
    if (self.access_flags.interface) utils.setPresent(u16, &access_flags_u, 0x0200);
    if (self.access_flags.abstract) utils.setPresent(u16, &access_flags_u, 0x0400);
    if (self.access_flags.synthetic) utils.setPresent(u16, &access_flags_u, 0x1000);
    if (self.access_flags.annotation) utils.setPresent(u16, &access_flags_u, 0x2000);
    if (self.access_flags.enum_class) utils.setPresent(u16, &access_flags_u, 0x4000);
    if (self.access_flags.module) utils.setPresent(u16, &access_flags_u, 0x8000);
    try writer.writeIntBig(u16, access_flags_u);

    try writer.writeIntBig(u16, self.this_class);
    try writer.writeIntBig(u16, self.super_class orelse 0);

    try writer.writeIntBig(u16, @intCast(u16, self.interfaces.items.len));
    for (self.interfaces.items) |i| try writer.writeIntBig(u16, i);

    try writer.writeIntBig(u16, @intCast(u16, self.fields.items.len));
    for (self.fields.items) |f| try f.encode(writer);

    try writer.writeIntBig(u16, @intCast(u16, self.methods.items.len));
    for (self.methods.items) |m| try m.encode(writer);

    try writer.writeIntBig(u16, @intCast(u16, self.attributes.items.len));
    for (self.attributes.items) |a| try a.encode(writer);
}

pub fn deinit(self: *ClassFile) void {
    self.constant_pool.deinit();
    self.interfaces.deinit();

    for (self.fields.items) |*fie| fie.deinit();
    self.fields.deinit();

    for (self.methods.items) |*met| met.deinit();
    self.methods.deinit();

    for (self.attributes.items) |*att| att.deinit();
    self.attributes.deinit();
}

pub const JavaSEVersion = enum { @"1.1", @"1.2", @"1.3", @"1.4", @"5.0", @"6", @"7", @"8", @"9", @"10", @"11", @"12", @"13", @"14", @"15", @"16" };
pub const GetJavaSEVersionError = error{InvalidMajorVersion};

/// Get the Java SE (or JDK for early versions) version corresponding to the ClassFile's `major_version` in accordance with [Table 4.1-A. class file format major versions](https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.1-200-B.2)
pub fn getJavaSEVersion(self: ClassFile) GetJavaSEVersionError!JavaSEVersion {
    return switch (self.major_version) {
        45 => .@"1.1",
        46 => .@"1.2",
        47 => .@"1.3",
        48 => .@"1.4",
        49 => .@"5.0",
        50 => .@"6",
        51 => .@"7",
        52 => .@"8",
        53 => .@"9",
        54 => .@"10",
        55 => .@"11",
        56 => .@"12",
        57 => .@"13",
        58 => .@"14",
        59 => .@"15",
        60 => .@"16",
        else => error.InvalidMajorVersion,
    };
}

// test "Decode ClassFile" {
//     const harness = @import("../test/harness.zig");
//     var reader = harness.hello.fbs().reader();

//     var cf = try ClassFile.decode(std.testing.allocator, reader);
//     defer cf.deinit();
// }

test "Encode ClassFile" {
    const harness = @import("../test/harness.zig");
    var reader = harness.hello.fbs().reader();

    var joe_file = try std.fs.cwd().createFile("Hello.class", .{});
    defer joe_file.close();

    var cf = try ClassFile.decode(std.testing.allocator, reader);
    defer cf.deinit();

    try cf.encode(joe_file.writer());
}
