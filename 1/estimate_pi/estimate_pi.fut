let estimate_pi [n] (xs: [n]f64) (ys: [n]f64) : f64 =
	let montecarlo (x: f64) (y: f64) =
		if ((x-1.0)**2.0) + ((y-1.0)**2.0) <= 1.0 then 1 else 0
	let mlist = map montecarlo xs ys
		in 4.0*(f64.i32 (length (filter (==1) mlist)) / f64.i32 (length mlist))


let main [n] (xs: [n]f64) (ys: [n]f64) : f64 =
	estimate_pi xs ys