const std = @import("std");

pub const TestFile = struct {
    data: []const u8,

    pub fn init(comptime name: []const u8) TestFile {
        return .{ .data = @embedFile(name ++ ".class") };
    }

    pub fn fbs(self: TestFile) std.io.FixedBufferStream([]const u8) {
        return std.io.fixedBufferStream(self.data);
    }
};

pub const hello = TestFile.init("Hello");
