const std = @import("std");

pub const attributes = @import("src/attributes.zig");
pub const ClassFile = @import("src/ClassFile.zig");
pub const ConstantPool = @import("src/ConstantPool.zig");
pub const descriptors = @import("src/descriptors.zig");
pub const FieldInfo = @import("src/FieldInfo.zig");
pub const MethodInfo = @import("src/MethodInfo.zig");
pub const bytecode = struct {
    pub const ops = @import("src/bytecode/ops.zig");
    pub const wrapped = @import("src/bytecode/wrapped.zig");
};

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(bytecode);
}
