-- literal 
int a = 1;
bool b = true;
bool c = false;
string str = "hello";
int list l1 = [1,2,3];
bool list l2 = [true, false, true, true];
int list list l3 = [[1,2], [3,4], [5,6]];
unit u = ();

-- datatype
datatype Color = Green of int | Red of int | Black | White;
datatype Background = Gradient of (int * Color) | Pure of Color; 
Background allBlack = (Pure (Black));
-- Background grey = (Gradient (50, (Black))); TODO: this is failing
Color green = (Green 100);
Color black = (Black);
datatype List = Empty | IntList of int list | StringList of string list;
List e = (Empty);
List i = (IntList [1,2,3]);
List s = (StringList ["c", "b", "c"]);

-- function and function application 
fun: int -> int:
f x = x + 1;
(f 1); 
(f a); 

fun: int -> int -> int:
add x y = x + y;
(add 1 2);
((add 1) 2);
int -> int add3 = (add 3);

-- if , begin, 
int x = (begin 1, 2, 3);
int y = if true then 1 else 2;

-- binop expression  
int num = 1 + 2;
num = (4 - 5) * 2 + 3;
num = (4 - 5) * (2 + 3);
num = 2 * 4;
num = 12839 / 234;
num = 398 % 32;
bool b = 3 == 3;
b = 4 != 3;
b = 1 < 2;
b = b or b;
b = b and b;
int list l = [1, 2, 3];
l = 1 :: l; 

-- unary 
int h = hd [1, 2, 3];
int list t = tl [1, 2, 3];
int negative = ~1;
bool bf = not true;

-- let expression 
int a = let int num1 = 1 in num1; 
bool b = let bool b = true in b and b; 


-- case expression  
datatype Shape = Circle | Square;
string shape = (case (Circle) of Circle => "circle" 
                               | Square => "square");
string shape = (case (Circle) of Circle => "circle" 
                               | _ => "square");

datatype Nat = Zero | TenTimesPlus of (Nat * int);
fun: Nat -> int:
last_digit n = (case n of Zero => 0 
                | TenTimesPlus (natural, d) =>  d);
int n = (case (Zero) of Zero => 0 
                     | TenTimesPlus (natural, d) =>  d);

-- fun: Nat -> int:
-- int_of_nat n = (case n of Zero => 0 
--                      | TenTimesPlus (natural, d) => (int_of_nat natural) * 10 + d)

datatype ColorScale = Red of int |  Blue of int ;
int scale = (case (Red 1) of Red n => n | Blue n => n);
Color color = (case (Pure (Black)) 
                of Pure c => c 
                |  Gradient (_ , c) => c);


