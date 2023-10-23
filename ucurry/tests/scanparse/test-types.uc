-- atoms
string x = "hello";
int x = 1 ;

-- lists 
int list vector = [1, 2, 3, 4];
bool list booleans = [true, false];
string list strings = ["h", "e"];
int list list matrix = [[1, 2], [3, 4]];
bool list list grids = [[true, false], [true, false]];
string list list paragraph = [["h", "e"], ["h", "e"]];
int list emptyList = [];
int list list emptyMatrix = [[], [], []];

-- tuples 
(int * string ) t = (1, 2, 3);
(int * int list * string) t2 = (1, [1 , 2], "hello");
(int * (int * int)) t3 = (1, (1, 2));

-- value constructor
Color blue = (Blue); 
Gradient blue_gradient = (Blue 1);

-- datatype 
datatype Natural = ZERO 
                 | TenTimesPlus of (Natural * int);

datatype Test =  List of int list
               | Tuple of (int * int * string)
               | Function of int -> int;

Test test1 = (List [1, 2, 3]);
Test test2 = (Tuple (1, 2, "pu-"));
Test test3 = (Function \(int -> int) a -> a);

-- function types
Color -> Color -> Color mixColor = \(Color -> Color -> Color) b c -> b + c ; 
(int * int) -> int add = \((int * int) -> int) a b -> a + b;
