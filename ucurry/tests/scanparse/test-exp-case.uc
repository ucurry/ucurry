(case color of 1 => 1);
(case 3 of _ => 1);
(case (Green) of Red => "red" | Green => "green" | Blue => "blue");
(case 0 of 0 => false | _ => true);
(case (Gray 100) of Gray x => x | White => 255 | Black => 0 );