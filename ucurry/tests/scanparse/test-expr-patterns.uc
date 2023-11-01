datatype Color = Green | Red | Blue; 
datatype GrayScale = Gray of int | White | Black;
(case 3 of _ => 1);
(case (Green) of Red => "red" | Green => "green" | Blue => "blue");
(case (Gray 100) of Gray x => x | White => 255 | Black => 0 );