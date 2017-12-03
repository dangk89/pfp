let mx (x:i32) (y:i32) : (i32) = -- Max function
	if x > y then x else y

let add_lists (xs:[]i32) (ys: []i32) : []i32 = -- Subtract signals pointwise and make absolute
 	map (\x -> i32.abs x) (map (-) xs ys)

let process [n] (xs: [n]i32) (ys: [n]i32): i32 = -- Process function
	reduce mx 0 (add_lists xs ys)

let main (xs: []i32) (ys: []i32) : i32 =
	process xs ys