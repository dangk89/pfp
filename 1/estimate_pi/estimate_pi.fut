default(f32)

-- Takes two arrays of f32 and estimates pi from the point-wise distance
let estimate_pi [n] (xs: [n]f32) (ys: [n]f32) : f32 =
	let montecarlo (x: f32) (y: f32) =
		if ((x-1.0)**2.0) + ((y-1.0)**2.0) <= 1.0 then 1 else 0
	let mlist = map montecarlo xs ys
		in 4.0*(f32.i32 (length (filter (==1) mlist)) / f32.i32 (length mlist))

let main [n] (xs: [n]f32) (ys: [n]f32) : f32 =
	estimate_pi xs ys