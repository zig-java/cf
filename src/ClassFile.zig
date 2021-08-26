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
/// The constant_pool table is indexed from 1 to constant_pool_count - 1
constant_pool: ConstantPool,
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

pub fn getConstantPoolEntry(self: ClassFile, index: u16) constant_pool_.Entry {
    return self.constant_pool[index - 1];
}

pub fn decode(allocator: *std.mem.Allocator, reader: anytype) !ClassFile {
    var magic = try reader.readIntBig(u32);
    if (magic != 0xCAFEBABE) return error.BadMagicValue;

    var minor_version = try reader.readIntBig(u16);
    var major_version = try reader.readIntBig(u16);

    var constant_pool = ConstantPool.init(allocator);
    try constant_pool.entries.ensureTotalCapacity((try reader.readIntBig(u16)) - 1);

    var constant_pool_index: usize = 0;
    while (constant_pool_index < constant_pool.entries.capacity) : (constant_pool_index += 1) {
        var cp = try constant_pool.decodeEntry(allocator, reader);
        constant_pool.entries.items[constant_pool_index] = cp;

        // Doubles and longs take up two slots because Java is bad (https://docs.oracle.com/javase/specs/jvms/se16/html/jvms-4.html#jvms-4.10.2.3)
        if (cp == .double or cp == .long) {
            constant_pool_index += 1;
        }
    }

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

    var interfaces_count = try reader.readIntBig(u16);
    var interfaces = try allocator.alloc(constant_pool_.ClassInfo, interfaces_count);
    for (interfaces) |*i| {
        var k = try reader.readStruct(constant_pool_.ClassInfo);
        if (std.Target.current.cpu.arch.endian() == .Little) {
            inline for (std.meta.fields(constant_pool_.ClassInfo)) |f2| @field(k, f2.name) = @byteSwap(f2.field_type, @field(k, f2.name));
        }
        i.* = k;
    }

    var fields_count = try reader.readIntBig(u16);
    var fieldss = try allocator.alloc(FieldInfo, fields_count);
    for (fieldss) |*f| f.* = try FieldInfo.readFrom(allocator, reader);

    var methods_count = try reader.readIntBig(u16);
    var methodss = try allocator.alloc(MethodInfo, methods_count);
    for (methodss) |*m| m.* = try MethodInfo.readFrom(allocator, reader);

    var attributes_count = try reader.readIntBig(u16);
    var attributess = try allocator.alloc(attributes.AttributeInfo, attributes_count);
    for (attributess) |*a| a.* = try attributes.AttributeInfo.readFrom(allocator, reader);

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

test "Decode ClassFile" {
    const harness = @import("../test/harness.zig");
    var reader = harness.hello.fbs().reader();

    std.debug.print("\n\n\n{s}\n\n\n", .{try ClassFile.decode(std.testing.allocator, reader)});
}
