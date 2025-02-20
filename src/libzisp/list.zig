const value = @import("value.zig");

const Value = value.Value;

pub fn reverse(list: Value) Value {
    return reverseWithTail(list, value.nil.nil);
}

pub fn reverseWithTail(list: Value, tail: Value) Value {
    var head = list;
    var result = tail;
    while (!value.nil.check(head)) {
        value.pair.assert(head);
        const car = value.pair.car(head);
        const cdr = value.pair.cdr(head);
        result = value.pair.cons(car, result);
        head = cdr;
    }
    return result;
}
