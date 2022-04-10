import "lib/github.com/diku-dk/segmented/segmented"

-- Based on https://github.com/nqpz/raster3d/ and Martin Elsman's
-- https://github.com/melsman/canvas demo.

type point = {x: i32, y: i32}

type triangle = (point, point, point)

type triangle_slopes = {n_lines: i32,
                        y: i32,
                        y_subtracted_p_y: {q: i32, r: i32},
                        p: i32,
                        r: i32,
                        s1: f32,
                        s2: f32,
                        s3: f32}

type line = {n_points: i32,
             y: i32,
             leftmost: i32,
             step: i32}

def bubble_point
    (a: point)
    (b: point): (point, point) =
  if b.y < a.y then (b, a) else (a, b)

def normalize_triangle_points ((p, q, r): triangle): triangle =
  let (p, q) = bubble_point p q
  let (q, r) = bubble_point q r
  let (p, q) = bubble_point p q
  in (p, q, r)

def slope (a: point) (b: point): f32 =
  let dy = b.y - a.y
  in if dy == 0
     then 0
     else r32 (b.x - a.x) / r32 dy

def triangle_slopes ((p, q, r): triangle): triangle_slopes =
  {n_lines=r.y - p.y + 1,
   y=p.y,
   y_subtracted_p_y={q=q.y - p.y,
                     r=r.y - p.y},
   p=p.x,
   r=r.x,
   s1=slope p q,
   s2=slope p r,
   s3= -slope q r}

def get_line_in_triangle 'a
    ((t, aux): (triangle_slopes, a))
    (i: i64): (line, a) =
  let i = i32.i64 i
  let y = t.y + i
  let half (p: i32) (s1: f32) (s2: f32) (i': f32): (line, a) =
    let x1 = p + t32 (f32.round (s1 * i'))
    let x2 = p + t32 (f32.round (s2 * i'))
    let step = i32.sgn (x2 - x1)
    let n_points = 1 + i32.abs (x2 - x1)
    in ({n_points, y, leftmost=x1, step}, aux)
  in if i <= t.y_subtracted_p_y.q
     then half t.p t.s1 t.s2 (r32 i) -- upper half
     else half t.r (-t.s2) t.s3 (r32 (t.y_subtracted_p_y.r - i)) -- lower half

def lines_of_triangles 'a [n]
    (triangles: [n]triangle_slopes)
    (aux: [n]a): [](line, a) =
  expand (\(t, _) -> i64.i32 t.n_lines) get_line_in_triangle (zip triangles aux)

def points_in_line 'a ((line, _): (line, a)): i64 =
  i64.i32 line.n_points

def get_point_in_line 'a ((l, aux): (line, a)) (i: i64): (point, a) =
  ({x=l.leftmost + l.step * i32.i64 i, y=l.y}, aux)

def points_of_lines 'a (lines: [](line, a)): [](point, a) =
  expand points_in_line get_point_in_line lines
