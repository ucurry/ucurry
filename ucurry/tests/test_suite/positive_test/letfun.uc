println (let f = \(int -> int) x -> x + 1 in (f 2));

int globalx = 5;

let f = \(int -> int) a -> a - globalx in println(f 10);
