-- basic support of let expression

-- local scope blocks global scope
int a = 1;
let int a = 2 in (println a);

-- nested let
let int x = 15 in 
    let int y = 25 in 
        let int z = 30 in 
            (println x + y + z);

let int x = 15, int y = 25, int z = 30 in (println x + y + z);
