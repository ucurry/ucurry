-- def -> expr -> case 
datatype Color = Green | Red | Black;
Color color = (Green);
(case color of a => 1);
(case color of Green => 1);
(case color of Green x => 1);

check_type_error (case alphaColor of 
    Green alpha => alpha 
  | Red alpha   => alpha); 

(case color of 
   Green => 1 
 | Red   => 2
 | Black => 3);
