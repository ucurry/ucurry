-- datatype with one argument
datatype Color = Gray of int | White of int;
Color grey = (Gray 0);
(White 255);
Color 

-- datatype with zero argument
datatype RGB = Red | Blue | Green; 
RGB r = (Red);
RGB b = (Blue);

-- datatype with tuple as argument 
datatype Person = Student of (string * int) 
                | Professor of string 
                | Speaker of (string * string list)
                | Teacher of string list;
Person somebody = (Student ("tom", 1));
Person prof = (Professor "NR");
Person speaker = (Speaker ("speaker1", ["cs117", "cs107"]));
Person teacher = (Teacher ["cs107", "cs112", "cs111"]);

-- datatype within datatype 
datatype Nested = NestedP of (Person * int) | NestedC of (Color * int);
Nested color_nested = (NestedC ((White 255), 1));
int z = ((White 255), 1).1;
println z;
println "datatype finished";

-- no syntax to support consuming a datatype; 
-- the only way to test is to see the generated llvm code 
