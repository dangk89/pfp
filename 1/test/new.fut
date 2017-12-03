
import "/futlib/math"

type point = (i32,i32)

let rotate_point (origin:point) ((x,y):point) (rads: f64) : (i32,i32) =
	let px = f64.i32 x
	let py = f64.i32 y
	let ox = f64.i32 origin.1
	let oy = f64.i32 origin.2
	let nx = ox + (f64.cos rads) * (px-ox) - (f64.sin rads) * (py - oy)
	let ny = oy + (f64.sin rads) * (px-ox) + (f64.cos rads) * (py - oy)
	in (i32.f64 nx, i32.f64 ny)

let main () : (i32,i32) =
	let o = (0,0)
	let p = (58,20)
	let r = 1.5
	in rotate_point o p r