fun: unit -> int :
idfun = 1;
println (idfun ());

--fun : unit -> (unit -> int):
--f = \(unit -> int) -> 1;
---- ((f ()) ());

-- -- TODO: automatic curry
-- fun: int -> int -> int :
-- f x y = x + y;

-- fun: int -> int:
-- addone = (f 1);
