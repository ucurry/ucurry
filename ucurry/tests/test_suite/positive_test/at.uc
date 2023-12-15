fun : (int * bool) -> int :
getInt t = t.0;
println (getInt (1, true));

fun : (int * bool) -> bool :
getBool t = t.1;
println (getBool (1, true));
