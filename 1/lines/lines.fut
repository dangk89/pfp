-- ==
-- input {} output @ lines_seq.ok

import "/futlib/math"

-- Finding points on a line
type point = (i32,i32)
type line = (point,point)
type points = []point

let compare (v1:i32) (v2:i32) : i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slo ((x1,y1):point) ((x2,y2):point) : f32 =
  if x2==x1 then if y2>y1 then 1f32 else -1f32
                 else r32(y2-y1) / f32.abs(r32(x2-x1))

let linepoints ((x1,y1):point, (x2,y2):point) : points =
  let len = 1 + i32.max (i32.abs(x2-x1)) (i32.abs(y2-y1))
  let xmax = i32.abs(x2-x1) > i32.abs(y2-y1)
  let (dir,slop) =
    if xmax then (compare x1 x2, slo (x1,y1) (x2,y2))
    else (compare y1 y2, slo (y1,x1) (y2,x2))
  in map (\i -> if xmax then (x1+i*dir, y1+t32(slop*r32(i)))
                else (x1+t32(slop*r32(i)), y1+i*dir)) (iota len)

-- Write to grid
let update [h] [w] [n] (grid:*[h][w]i32)(xs:[n]i32)(ys:[n]i32):*[h][w]i32 =
  let is = map (\x y -> w*y+x) xs ys
  let flatgrid = reshape (h*w) grid
  let ones = map (\ _ -> 1) is
  in reshape (h,w) (scatter flatgrid is ones)

-- Sequential algorithm for drawing multiple lines
let drawlines [h] [w] [n] (grid: *[h][w]i32) (lines:[n]line) : [h][w]i32 =
  loop (grid) for i < n do -- find points for line i
    let (xs,ys) = unzip (linepoints (lines[i]))
    in update grid xs ys

-- Translation point
let transl_point ((xoff,yoff):point) ((x,y):point) : point =
  ((xoff+x),(yoff+y))

-- Translate image
let transl_img ((xoff,yoff):point) ((t1,t2):line) : line = 
	(transl_point (xoff,yoff) t1, transl_point (xoff,yoff) t2)

-- Rotate point
let rotate_point (rpoint:point) (rads: f32) ((x,y):point)  : point =
	 -- Convert all vars to float
  let px = f32.i32 x 
	let py = f32.i32 y
	let ox = f32.i32 rpoint.1
	let oy = f32.i32 rpoint.2
  -- Calculate new points
	let nx = f32.round(ox + (f32.cos rads) * (px-ox) - (f32.sin rads) * (py - oy))
	let ny = f32.round(oy + (f32.sin rads) * (px-ox) + (f32.cos rads) * (py - oy))
	in (i32.f32 nx, i32.f32 ny)

-- Rotate image
let rotate_img (rpoint:point) (rads: f32) ((t1,t2):line) : line =
  -- Function that can be mapped on type []line
	(rotate_point rpoint rads t1,  rotate_point rpoint rads t2) 

-- Draw lines on a 70 by 30 grid
let main () : [][]i32 =
  let height:i32 = 30
  let width:i32 = 70
  let grid : *[][]i32 = replicate height (replicate width 0)
  --let lines = [((58,20),(2,3)),((11,25),(5,15))]
  let lines = [((1,1),(1,70))]
  --let transl_lines = map (transl_img (1,1)) lines
  let rot_lines = map (rotate_img (15,35) 1.570) lines
 
  in drawlines grid rot_lines