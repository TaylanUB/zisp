// See parse.zig for details.

const parser = @import("parser.zig");
const decoder = @import("decoder.zig");

const Value = @import("../value.zig").Value;

pub fn readCode(input: []const u8) Value {
    return decoder.decode(parser.parse(input, .code));
}
