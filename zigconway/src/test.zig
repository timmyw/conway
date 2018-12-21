const assert = @import("std").debug.assert;

var some_integers: [100]i32 = undefined;

test "declaring an array" {
    for (some_integers) |*item, i| {
        *item = i32(i);
    }
    assert(some_integers[10] == 10);
    assert(some_integers[99] == 99);
}
