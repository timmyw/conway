const warn = @import("std").debug.warn;
const Board = @import("./board.zig").Board;

pub fn main() void {
    warn("conway\n");

    var b = Board.init(20, 20);
}

