default(f32)

let volume [n] (xs: [n]f32) (ys: [n]f32) : f32 =
	let f ( x : f32 ) ( y : f32 ) : f32 =
	2.0 * x * x * x * x * x * x * y * y - x * x * x * x * x * x * y
	+ 3.0 * x * x * x * y * y * y - x * x * y * y * y +
	x * x * x * y - 3.0 * x * y * y + x * y -
	5.0 * y + 2.0 * x * x * x * x * x * y * y * y * y -
	2.0 * x * x * x * x * x * y * y * y * y * y + 250.0

	in (4.0/f32.i32 n) * (reduce (+) 0.0 (map f xs ys))

let main [n] (xs: [n]f32) (ys: [n]f32) : f32 =
	volume xs ys