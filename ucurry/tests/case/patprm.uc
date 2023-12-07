datatype Color = Green | Red | Blue;
println (Green).T;
println (Red).T;
println (Blue).T;

datatype Gradient = Black of int | White of int;
println (Black 100)@1;
println ((White 4)@2);
