-- basic support of let expression

-- local scope blocks global scope
int a = 1;
let  a = 2 in (println a);

-- nested let
let  x = 15 in 
    let  y = 25 in 
        let  z = 30 in 
            (println x + y + z);

let x = 15,  y = 25,  z = 30 in (println x + y + z);
