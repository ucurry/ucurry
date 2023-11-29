-- value constructor `IntList` expects an int list but receives a string list  
datatype List = Empty | IntList of int list | StringList of string list;
List i = (IntList ["c", "d"]);