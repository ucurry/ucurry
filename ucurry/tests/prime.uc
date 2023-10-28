fun: int list -> int list:
sieve l = let int p = hd l, int list xs = tl l in 
					p :: (sieve (filter (\(int -> bool)x -> x mod p != 0) xs));