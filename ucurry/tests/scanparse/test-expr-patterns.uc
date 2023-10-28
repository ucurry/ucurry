(case color of a => 1);
(case 3 of _ => 1);
(case (Green) of Red => "red" | Green => "green" | Blue => "blue");
(case 0 of a => false | _ => true);
(case (Gray 100) of Gray x => x | White => 255 | Black => 0 );