-- list cannot contain elements of different types 
[1, true, false, "hello"];

-- case expression on list does not match with the type of the scrutinee 
case ["hello", "world"] of 
  | [] -> 0
  | hd :: tl -> hd + 10;

-- value constructor `IntList` expects an int list but receives a string list  
datatype List = Empty | IntList of int list | StringList of string list;
check_type_error List i = (IntList ["c", "d"]);