pub fn isPresent(comptime T: type, u: T, c: T) bool {
    return u & c == c;
}

pub fn setPresent(comptime T: type, value: *T, thang: T) void {
    value.* |= thang;
}
