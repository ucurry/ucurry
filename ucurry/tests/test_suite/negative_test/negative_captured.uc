-- the following code should fail the semantic check in `clconvert.ml`
-- The reason is that in the capture list of a closure, we copy the
-- value rather than the reference. To enable mutating a captured variable,
-- we need to move the variable to the heap given that the value inside
-- the variable is unknown (and the capture list should be modified to store
-- references rather than values)

-- If we remove this semantics check (line 90 in `clconvert.ml`), codegen will
-- fail because we only maintain variable map for formals and locals when
-- generating a function body (line 266-8 in `codegen.ml`)

-- We are able to mutate local variables in the closure (the ones created by
-- a lambda or a let (see `set-local.uc`)) becuase their heap location is known 
-- (its just the input parameter).

int x = 1;
fun: int -> int:
f a = x = 1;
(f 1);
println x;
