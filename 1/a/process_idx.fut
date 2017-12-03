let mx_idx (x:i32,i1:i32) (y:i32,i2:i32) : (i32,i32) = -- Max function taking tuples (val, idx)
	if x > y then (x,i1) else (y,i2)

let add_lists (xs:[]i32) (ys: []i32) : []i32 = -- Subtract signals pointwise and make absolute
 	map (\x -> i32.abs x) (map (-) xs ys)

let process_idx [n] (xs: [n]i32) (ys: [n]i32) : (i32,i32) = -- Process function that returns index as well
	reduce mx_idx (0, -1) (zip (add_lists xs ys) (iota n))  --Use 0 as min value as we are dealing with absolute values

let main (xs: []i32) (ys: []i32) : (i32,i32) =
	process_idx xs ys