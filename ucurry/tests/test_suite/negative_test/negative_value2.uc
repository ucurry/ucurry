-- case expression on list does not match with the type of the scrutinee 
(case ["hello", "world"] of 
    [] => 0
  | y :: ys => y + 10);