datatype Color = Gray of int | White of int;
Color grey = (Gray 0);
(White 255);
datatype RGB = Red | Blue | Green; 
RGB r = (Red);
RGB b = (Blue);
println "datatype finished";
-- no syntax to support consuming a datatype; 
-- the only way to test is to see the generated llvm code 
