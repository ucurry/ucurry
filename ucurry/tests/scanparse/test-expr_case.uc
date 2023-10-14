-- def -> expr -> case
case color of 1 => 1;
case color of Green => 1;
case color of Green x => 1; 

case color of 
   Green => 1 
 | Red   => 2 
 | Black => 3;

case alphaColor of 
    Green alpha => alpha 
  | Red alpha   => alpha ; 
