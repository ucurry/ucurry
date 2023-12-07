datatype Color = Green of unit | Red | Blue;
println (Green).T;
println (Red ()).T;
println (Blue).T;

datatype Gradient = Black of int | White of int;
println (Black 100)@1;
println ((White 4)@2);

if (Black 100).T == "White" 
    then let a = (Black 100)@1 in (println a)
    else (println 200);

if (Black 100).T == "Black" 
    then let a = (Black 100)@1 in (println a)
    else (println 200);