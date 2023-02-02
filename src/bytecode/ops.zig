pub const Opcode = enum(u8) {
    nop = 0x00,
    aconst_null = 0x01,
    iconst_m1 = 0x02,
    iconst_0 = 0x03,
    iconst_1 = 0x04,
    iconst_2 = 0x05,
    iconst_3 = 0x06,
    iconst_4 = 0x07,
    iconst_5 = 0x08,
    lconst_0 = 0x09,
    lconst_1 = 0x0a,
    fconst_0 = 0x0b,
    fconst_1 = 0x0c,
    fconst_2 = 0x0d,
    dconst_0 = 0x0e,
    dconst_1 = 0x0f,
    bipush = 0x10,
    sipush = 0x11,
    ldc = 0x12,
    ldc_w = 0x13,
    ldc2_w = 0x14,
    iload = 0x15,
    lload = 0x16,
    fload = 0x17,
    dload = 0x18,
    aload = 0x19,
    iload_0 = 0x1a,
    iload_1 = 0x1b,
    iload_2 = 0x1c,
    iload_3 = 0x1d,
    lload_0 = 0x1e,
    lload_1 = 0x1f,
    lload_2 = 0x20,
    lload_3 = 0x21,
    fload_0 = 0x22,
    fload_1 = 0x23,
    fload_2 = 0x24,
    fload_3 = 0x25,
    dload_0 = 0x26,
    dload_1 = 0x27,
    dload_2 = 0x28,
    dload_3 = 0x29,
    aload_0 = 0x2a,
    aload_1 = 0x2b,
    aload_2 = 0x2c,
    aload_3 = 0x2d,
    iaload = 0x2e,
    laload = 0x2f,
    faload = 0x30,
    daload = 0x31,
    aaload = 0x32,
    baload = 0x33,
    caload = 0x34,
    saload = 0x35,
    istore = 0x36,
    lstore = 0x37,
    fstore = 0x38,
    dstore = 0x39,
    astore = 0x3a,
    istore_0 = 0x3b,
    istore_1 = 0x3c,
    istore_2 = 0x3d,
    istore_3 = 0x3e,
    lstore_0 = 0x3f,
    lstore_1 = 0x40,
    lstore_2 = 0x41,
    lstore_3 = 0x42,
    fstore_0 = 0x43,
    fstore_1 = 0x44,
    fstore_2 = 0x45,
    fstore_3 = 0x46,
    dstore_0 = 0x47,
    dstore_1 = 0x48,
    dstore_2 = 0x49,
    dstore_3 = 0x4a,
    astore_0 = 0x4b,
    astore_1 = 0x4c,
    astore_2 = 0x4d,
    astore_3 = 0x4e,
    iastore = 0x4f,
    lastore = 0x50,
    fastore = 0x51,
    dastore = 0x52,
    aastore = 0x53,
    bastore = 0x54,
    castore = 0x55,
    sastore = 0x56,
    pop = 0x57,
    pop2 = 0x58,
    dup = 0x59,
    dup_x1 = 0x5a,
    dup_x2 = 0x5b,
    dup2 = 0x5c,
    dup2_x1 = 0x5d,
    dup2_x2 = 0x5e,
    swap = 0x5f,
    iadd = 0x60,
    ladd = 0x61,
    fadd = 0x62,
    dadd = 0x63,
    isub = 0x64,
    lsub = 0x65,
    fsub = 0x66,
    dsub = 0x67,
    imul = 0x68,
    lmul = 0x69,
    fmul = 0x6a,
    dmul = 0x6b,
    idiv = 0x6c,
    ldiv = 0x6d,
    fdiv = 0x6e,
    ddiv = 0x6f,
    irem = 0x70,
    lrem = 0x71,
    frem = 0x72,
    drem = 0x73,
    ineg = 0x74,
    lneg = 0x75,
    fneg = 0x76,
    dneg = 0x77,
    ishl = 0x78,
    lshl = 0x79,
    ishr = 0x7a,
    lshr = 0x7b,
    iushr = 0x7c,
    lushr = 0x7d,
    iand = 0x7e,
    land = 0x7f,
    ior = 0x80,
    lor = 0x81,
    ixor = 0x82,
    lxor = 0x83,
    iinc = 0x84,
    i2l = 0x85,
    i2f = 0x86,
    i2d = 0x87,
    l2i = 0x88,
    l2f = 0x89,
    l2d = 0x8a,
    f2i = 0x8b,
    f2l = 0x8c,
    f2d = 0x8d,
    d2i = 0x8e,
    d2l = 0x8f,
    d2f = 0x90,
    i2b = 0x91,
    i2c = 0x92,
    i2s = 0x93,
    lcmp = 0x94,
    fcmpl = 0x95,
    fcmpg = 0x96,
    dcmpl = 0x97,
    dcmpg = 0x98,
    ifeq = 0x99,
    ifne = 0x9a,
    iflt = 0x9b,
    ifge = 0x9c,
    ifgt = 0x9d,
    ifle = 0x9e,
    if_icmpeq = 0x9f,
    if_icmpne = 0xa0,
    if_icmplt = 0xa1,
    if_icmpge = 0xa2,
    if_icmpgt = 0xa3,
    if_icmple = 0xa4,
    if_acmpeq = 0xa5,
    if_acmpne = 0xa6,
    goto = 0xa7,
    jsr = 0xa8,
    ret = 0xa9,
    tableswitch = 0xaa,
    lookupswitch = 0xab,
    ireturn = 0xac,
    lreturn = 0xad,
    freturn = 0xae,
    dreturn = 0xaf,
    areturn = 0xb0,
    @"return" = 0xb1,
    getstatic = 0xb2,
    putstatic = 0xb3,
    getfield = 0xb4,
    putfield = 0xb5,
    invokevirtual = 0xb6,
    invokespecial = 0xb7,
    invokestatic = 0xb8,
    invokeinterface = 0xb9,
    invokedynamic = 0xba,
    new = 0xbb,
    newarray = 0xbc,
    anewarray = 0xbd,
    arraylength = 0xbe,
    athrow = 0xbf,
    checkcast = 0xc0,
    instanceof = 0xc1,
    monitorenter = 0xc2,
    monitorexit = 0xc3,
    wide = 0xc4,
    multianewarray = 0xc5,
    ifnull = 0xc6,
    ifnonnull = 0xc7,
    goto_w = 0xc8,
    jsr_w = 0xc9,
    breakpoint = 0xca,
    impdep1 = 0xfe,
    impdep2 = 0xff,

    pub fn resolve(op: u8) Opcode {
        return @intToEnum(Opcode, op);
    }
};

pub const LocalIndexOperation = u16; // u16 instead of u8 to account for wide
pub const ConstantPoolRefOperation = u16;
pub const BranchToOffsetOperation = i16;
pub const BranchToOffsetWideOperation = i32;

pub const BipushParams = i8;
pub const SipushParams = i16;

pub const IincParams = struct {
    const Self = @This();

    index: LocalIndexOperation,
    @"const": i16,
};

pub const InvokeDynamicParams = struct {
    const Self = @This();

    index: u16,
    pad: u16,

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Self {
        _ = allocator;

        return Self{
            .index = try reader.readIntBig(u16),
            .pad = try reader.readIntBig(u16),
        };
    }

    pub fn encode(self: Self, writer: anytype) !void {
        try writer.writeIntBig(u16, self.index);
        try writer.writeIntBig(u16, self.pad);
    }
};

pub const InvokeInterfaceParams = struct {
    const Self = @This();

    index: u16,
    count: u8,
    pad: u8 = 0,

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Self {
        _ = allocator;

        return Self{
            .index = try reader.readIntBig(u16),
            .count = try reader.readByte(),
            .pad = try reader.readByte(),
        };
    }

    pub fn encode(self: Self, writer: anytype) !void {
        try writer.writeIntBig(u16, self.index);
        try writer.writeByte(self.count);
        try writer.writeByte(self.pad);
    }
};

pub const LookupPair = struct {
    match: i32,
    offset: i32,
};

pub const LookupSwitchParams = struct {
    const Self = @This();

    skipped_bytes: usize,
    default_offset: i32,
    pairs: []LookupPair,

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Self {
        var skipped_bytes = std.mem.alignForward(reader.context.pos, 4) - reader.context.pos;
        try reader.skipBytes(skipped_bytes, .{});

        const default_offset = try reader.readIntBig(i32);
        const npairs = try reader.readIntBig(i32);

        var pairs = try allocator.alloc(LookupPair, @intCast(usize, npairs));
        for (pairs) |*pair|
            pair.* = LookupPair{
                .match = try reader.readIntBig(i32),
                .offset = try reader.readIntBig(i32),
            };

        return Self{
            .skipped_bytes = skipped_bytes,
            .default_offset = default_offset,
            .pairs = pairs,
        };
    }

    pub fn encode(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // TODO: Encode
    }
};

pub const TableSwitchParams = struct {
    const Self = @This();

    skipped_bytes: usize,
    default_offset: i32,
    low: i32,
    high: i32,
    jumps: []i32,

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Self {
        var skipped_bytes = std.mem.alignForward(reader.context.pos, 4) - reader.context.pos;
        try reader.skipBytes(skipped_bytes, .{});

        const default_offset = try reader.readIntBig(i32);
        const low = try reader.readIntBig(i32);
        const high = try reader.readIntBig(i32);

        var jumps = try allocator.alloc(i32, @intCast(usize, high - low + 1));
        for (jumps) |*jump|
            jump.* = try reader.readIntBig(i32);

        return Self{
            .skipped_bytes = skipped_bytes,
            .default_offset = default_offset,
            .low = low,
            .high = high,
            .jumps = jumps,
        };
    }

    pub fn encode(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // TODO: Encode
    }
};

pub const MultiANewArrayParams = struct {
    const Self = @This();

    index: ConstantPoolRefOperation,
    dimensions: u8,

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Self {
        _ = allocator;
        return Self{
            .index = try reader.readIntBig(u16),
            .dimensions = try reader.readByte(),
        };
    }

    pub fn encode(self: Self, writer: anytype) !void {
        _ = self;
        _ = writer;
        // TODO: Encode
    }
};

pub const NewArrayParams = enum(u8) {
    boolean = 4,
    char = 5,
    float = 6,
    double = 7,
    byte = 8,
    short = 9,
    int = 10,
    long = 11,
};

pub const Operation = union(Opcode) {
    aload: LocalIndexOperation,
    anewarray: ConstantPoolRefOperation,
    astore: LocalIndexOperation,
    bipush: BipushParams,
    checkcast: ConstantPoolRefOperation,
    dload: LocalIndexOperation,
    dstore: LocalIndexOperation,
    fload: LocalIndexOperation,
    fstore: LocalIndexOperation,
    getfield: ConstantPoolRefOperation,
    getstatic: ConstantPoolRefOperation,

    goto: BranchToOffsetOperation,
    if_acmpeq: BranchToOffsetOperation,
    if_acmpne: BranchToOffsetOperation,
    if_icmpeq: BranchToOffsetOperation,
    if_icmpge: BranchToOffsetOperation,
    if_icmpgt: BranchToOffsetOperation,
    if_icmple: BranchToOffsetOperation,
    if_icmplt: BranchToOffsetOperation,
    if_icmpne: BranchToOffsetOperation,
    ifeq: BranchToOffsetOperation,
    ifge: BranchToOffsetOperation,
    ifgt: BranchToOffsetOperation,
    ifle: BranchToOffsetOperation,
    iflt: BranchToOffsetOperation,
    ifne: BranchToOffsetOperation,
    ifnonnull: BranchToOffsetOperation,
    ifnull: BranchToOffsetOperation,
    jsr: BranchToOffsetOperation,

    goto_w: BranchToOffsetWideOperation,
    jsr_w: BranchToOffsetWideOperation,

    iinc: IincParams,
    iload: LocalIndexOperation,
    instanceof: ConstantPoolRefOperation,
    invokedynamic: InvokeDynamicParams,
    invokeinterface: InvokeInterfaceParams,
    invokespecial: ConstantPoolRefOperation,
    invokestatic: ConstantPoolRefOperation,
    invokevirtual: ConstantPoolRefOperation,
    istore: LocalIndexOperation,
    ldc: u8, // NOTE: This is not a local variable! It's probably for compat
    ldc_w: ConstantPoolRefOperation,
    ldc2_w: ConstantPoolRefOperation,
    lookupswitch: LookupSwitchParams,
    tableswitch: TableSwitchParams,
    new: ConstantPoolRefOperation,
    multianewarray: MultiANewArrayParams,
    lload: LocalIndexOperation,
    lstore: LocalIndexOperation,
    sipush: SipushParams,
    putstatic: ConstantPoolRefOperation,
    putfield: ConstantPoolRefOperation,
    newarray: NewArrayParams,

    nop: void,
    aconst_null: void,
    iconst_m1: void,
    iconst_0: void,
    iconst_1: void,
    iconst_2: void,
    iconst_3: void,
    iconst_4: void,
    iconst_5: void,
    lconst_0: void,
    lconst_1: void,
    fconst_0: void,
    fconst_1: void,
    fconst_2: void,
    dconst_0: void,
    dconst_1: void,
    iload_0: void,
    iload_1: void,
    iload_2: void,
    iload_3: void,
    lload_0: void,
    lload_1: void,
    lload_2: void,
    lload_3: void,
    fload_0: void,
    fload_1: void,
    fload_2: void,
    fload_3: void,
    dload_0: void,
    dload_1: void,
    dload_2: void,
    dload_3: void,
    aload_0: void,
    aload_1: void,
    aload_2: void,
    aload_3: void,
    iaload: void,
    laload: void,
    faload: void,
    daload: void,
    aaload: void,
    baload: void,
    caload: void,
    saload: void,
    istore_0: void,
    istore_1: void,
    istore_2: void,
    istore_3: void,
    lstore_0: void,
    lstore_1: void,
    lstore_2: void,
    lstore_3: void,
    fstore_0: void,
    fstore_1: void,
    fstore_2: void,
    fstore_3: void,
    dstore_0: void,
    dstore_1: void,
    dstore_2: void,
    dstore_3: void,
    astore_0: void,
    astore_1: void,
    astore_2: void,
    astore_3: void,
    iastore: void,
    lastore: void,
    fastore: void,
    dastore: void,
    aastore: void,
    bastore: void,
    castore: void,
    sastore: void,
    pop: void,
    pop2: void,
    dup: void,
    dup_x1: void,
    dup_x2: void,
    dup2: void,
    dup2_x1: void,
    dup2_x2: void,
    swap: void,
    iadd: void,
    ladd: void,
    fadd: void,
    dadd: void,
    isub: void,
    lsub: void,
    fsub: void,
    dsub: void,
    imul: void,
    lmul: void,
    fmul: void,
    dmul: void,
    idiv: void,
    ldiv: void,
    fdiv: void,
    ddiv: void,
    irem: void,
    lrem: void,
    frem: void,
    drem: void,
    ineg: void,
    lneg: void,
    fneg: void,
    dneg: void,
    ishl: void,
    lshl: void,
    ishr: void,
    lshr: void,
    iushr: void,
    lushr: void,
    iand: void,
    land: void,
    ior: void,
    lor: void,
    ixor: void,
    lxor: void,
    i2l: void,
    i2f: void,
    i2d: void,
    l2i: void,
    l2f: void,
    l2d: void,
    f2i: void,
    f2l: void,
    f2d: void,
    d2i: void,
    d2l: void,
    d2f: void,
    i2b: void,
    i2c: void,
    i2s: void,
    lcmp: void,
    fcmpl: void,
    fcmpg: void,
    dcmpl: void,
    dcmpg: void,
    ret: void,
    ireturn: void,
    lreturn: void,
    freturn: void,
    dreturn: void,
    areturn: void,
    @"return": void,
    arraylength: void,
    athrow: void,
    monitorenter: void,
    monitorexit: void,
    wide: void,
    breakpoint: void,
    impdep1: void,
    impdep2: void,

    pub fn sizeOf(self: Operation) usize {
        inline for (std.meta.fields(Operation)) |op| {
            if (@enumToInt(std.meta.stringToEnum(Opcode, op.name).?) == @enumToInt(self)) {
                return 1 + if (op.type == void) 0 else @sizeOf(op.type);
            }
        }

        unreachable;
    }

    // TODO: Implement wide iirc
    const widenable = &[_]Opcode{
        .iload,
        .fload,
        .aload,
        .lload,
        .dload,
        .istore,
        .fstore,
        .astore,
        .lstore,
        .dstore,
    };

    pub fn decode(allocator: std.mem.Allocator, reader: anytype) !Operation {
        var opcode = try reader.readIntBig(u8);
        if (opcode == @enumToInt(Opcode.wide)) {
            var widened_opcode = try reader.readIntBig(u8);

            if (widened_opcode == @enumToInt(Opcode.iinc)) {
                return Operation{ .iinc = .{ .index = try reader.readIntBig(u16), .@"const" = try reader.readIntBig(i16) } };
            }

            inline for (widenable) |op| {
                if (@enumToInt(op) == widened_opcode) {
                    return @unionInit(Operation, @tagName(op), try reader.readIntBig(u16));
                }
            }
        } else {
            if (opcode == @enumToInt(Opcode.iinc)) {
                return Operation{ .iinc = .{ .index = try reader.readIntBig(u8), .@"const" = try reader.readIntBig(i8) } };
            }

            inline for (widenable) |op| {
                if (@enumToInt(op) == opcode) {
                    return @unionInit(Operation, @tagName(op), try reader.readIntBig(u8));
                }
            }

            inline for (std.meta.fields(Operation)) |op| {
                if (@enumToInt(std.meta.stringToEnum(Opcode, op.name).?) == opcode) {
                    return @unionInit(Operation, op.name, if (op.type == void) {} else if (@typeInfo(op.type) == .Struct) z: {
                        break :z if (@hasDecl(op.type, "decode")) try @field(op.type, "decode")(allocator, reader) else unreachable;
                    } else if (@typeInfo(op.type) == .Enum) try reader.readEnum(op.type, .Big) else if (@typeInfo(op.type) == .Int) try reader.readIntBig(op.type) else unreachable);
                }
            }
        }

        unreachable;
    }

    pub fn encode(self: Operation, writer: anytype) !void {
        if (self == .iinc) {
            var iinc = self.iinc;
            if (iinc.index > std.math.maxInt(u8) or iinc.@"const" > std.math.maxInt(i8) or iinc.@"const" < std.math.minInt(i8)) {
                try writer.writeByte(@enumToInt(Opcode.wide));
                try writer.writeByte(@enumToInt(Opcode.iinc));
                try writer.writeIntBig(u16, iinc.index);
                try writer.writeIntBig(i16, iinc.@"const");
                return;
            } else {
                try writer.writeByte(@enumToInt(Opcode.iinc));
                try writer.writeByte(@intCast(u8, iinc.index));
                try writer.writeIntBig(i8, @intCast(i8, iinc.@"const"));
                return;
            }
        }

        inline for (widenable) |op| {
            if (@enumToInt(op) == @enumToInt(self)) {
                var v = @field(self, @tagName(op));

                if (v > std.math.maxInt(u8)) {
                    try writer.writeByte(@enumToInt(Opcode.wide));
                    try writer.writeByte(@enumToInt(self));
                    try writer.writeIntBig(u16, v);
                    return;
                } else {
                    try writer.writeByte(@enumToInt(self));
                    try writer.writeIntBig(u8, @intCast(u8, v));
                    return;
                }
            }
        }

        try writer.writeByte(@enumToInt(self));

        inline for (std.meta.fields(Operation)) |op| {
            if (@enumToInt(std.meta.stringToEnum(Opcode, op.name).?) == @enumToInt(self)) {
                switch (op.type) {
                    void => {},
                    else => switch (@typeInfo(op.type)) {
                        .Struct => if (@hasDecl(op.type, "encode")) try @field(@field(self, op.name), "encode")(writer) else unreachable,
                        .Enum => try writer.writeIntBig(@typeInfo(op.type).Enum.tag_type, @enumToInt(@field(self, op.name))),
                        .Int => try writer.writeIntBig(op.type, @field(self, op.name)),
                        else => unreachable,
                    },
                }
            }
        }
    }
};

const std = @import("std");

/// Get the index specified by an operation, if possible
pub fn getIndex(inst: anytype) u16 {
    if (!@hasField(@TypeOf(inst), "indexbyte1")) @compileError("This instruction does not have an index!");
    return @intCast(u16, @field(inst, "indexbyte1")) << @intCast(u16, 8) | @intCast(u16, @field(inst, "indexbyte2"));
}

test "Decode and encode opcodes including wides" {
    const ClassFile = @import("../ClassFile.zig");
    const harness = @import("../../test/harness.zig");
    var wide_fbs = harness.wide.fbs();
    var reader = wide_fbs.reader();

    var cf = try ClassFile.decode(std.testing.allocator, reader);
    defer cf.deinit();

    for (cf.methods.items) |method| {
        if (std.mem.eql(u8, "main", method.getName().bytes)) {
            for (method.attributes.items) |attr| {
                if (attr == .code) {
                    var final = try std.testing.allocator.alloc(u8, attr.code.code.items.len);
                    defer std.testing.allocator.free(final);

                    var fbs = std.io.fixedBufferStream(attr.code.code.items);
                    var final_fbs = std.io.fixedBufferStream(final);

                    while (true) {
                        var op = try Operation.decode(std.testing.allocator, fbs.reader());
                        try op.encode(final_fbs.writer());
                        if (op == .@"return") break;
                    }

                    try std.testing.expectEqualSlices(u8, attr.code.code.items, final);
                }
            }
        }
    }
}
