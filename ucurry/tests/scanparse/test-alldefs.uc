int x = 1; -- THIS IS A COMMENT
fun: int -> int:
f x = x + 1;
(f 1);
1;
"hello-world";


-- def -> datatype
datatype Color = Green; 
datatype AlphaColor = Green of int | Red of int | Black of int ; 
datatype Background = Pure of Color | Number of int ;

-- def -> type var = a
int a = 1;
bool b = true;


-- def -> fun
fun: int -> int:
f x = x + 1;-- def -> expr -> binop
1 + 2;
2 * 4;
2 >= 4;-- def -> expr -> case
case color of 1 => 1;
case color of Green => 1;
case color of Green x => 1; 