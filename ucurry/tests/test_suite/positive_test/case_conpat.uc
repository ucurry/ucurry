datatype Bit = ZERO | ONE; 
datatype OP = OR of (Bit * Bit) | AND of (Bit * Bit); 

OP zero = (OR ((ZERO), (ZERO)));
OP one = (OR ((ONE), (ZERO)));
OP oneIII = (OR ((ONE), (ONE)));

OP zeroIII = (AND ((ONE), (ZERO)));
OP zeroII = (AND ((ZERO), (ONE)));
OP oneII = (AND ((ONE), (ONE)));


fun : OP -> int : 
f b = (case b of 
            OR (ZERO, ZERO) => 0 
          | OR _ => 1 
          | AND (ONE, ONE) => 1
          | AND _ => 0);

println (f zero);
println (f zeroII);
println (f zeroIII); 
println (f one); 
println (f oneII);
println (f oneIII);

fun : (Bit * Bit) -> int : 
b bits = (case bits of 
                 (ZERO, ZERO) => 0 
               | (ZERO, ONE) => 1 
               | (ONE, ZERO) => 2 
               | (ONE, ONE) => 3);

println (b ((ZERO), (ZERO)));
println (b ((ZERO), (ONE)));
println (b ((ONE), (ZERO)));
println (b ((ONE), (ONE)));