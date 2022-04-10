import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "scanline"

module vec2 = mk_vspace_2d f32

type position = vec2.vector

type particle = {basis_distance: f32, basis_angle: f32}

type basis = {position: position, orientation: f32}

type cluster [n] = {basis: basis, particles: [n]particle}

def radius = 10i64
def diameter = radius * 2

def adjust_basis [n] (changes: [n](particle, position)) (basis: basis): basis =
  let calc (particle, position_change) =
    let pos_of_angle basis_angle =
      let a = basis_angle + basis.orientation
      in {x=particle.basis_distance * f32.cos a,
          y=particle.basis_distance * f32.sin a}
    let {x, y} = pos_of_angle particle.basis_angle
    let (x', y') = (x + position_change.x, y + position_change.y)
    let basis_angle' = f32.atan2 y' x' - basis.orientation
    let angle_diff = basis_angle' - particle.basis_angle
    in if f32.abs angle_diff > 1 then 0 else angle_diff -- hack, fixme

  let angle_diffs = map calc changes
  in basis with orientation = basis.orientation + f32.sum angle_diffs
           with position.x = basis.position.x + f32.sum (map (.1.x) changes)
           with position.y = basis.position.y + f32.sum (map (.1.y) changes)

--mk_triangle

def mk_square (height: i64) (width: i64): cluster [] =
  let basis = {position={x=f32.i64 (diameter * width) / 2, y=f32.i64 (diameter * height) / 2}, orientation=0}
  let mk_particle y x =
    let x = x - basis.position.x
    let y = y - basis.position.y
    in {basis_distance=f32.sqrt (x * x + y * y),
        basis_angle=f32.atan2 y x}
  let particles = tabulate_2d height width (\y x -> mk_particle (f32.i64 (diameter * y)) (f32.i64 (diameter * x)))
                  |> flatten
  in {basis, particles}

def circle_points: [](i64, i64) =
  let is_in_circle (y, x) =
    let y' = f32.i64 (y - radius)
    let x' = f32.i64 (x - radius)
    in f32.sqrt (y' * y' + x' * x') < f32.i64 radius
  in tabulate_2d diameter diameter (\y x -> (y, x))
     |> flatten
     |> filter is_in_circle

def triangle_points [n] (triangle_slopes: [n]triangle_slopes) =
  let lines = lines_of_triangles triangle_slopes (replicate n ())
  in points_of_lines lines

type text_content = (i32, f32, f32)
module lys: lys with text_content = text_content = {
  type~ state = {time: f32, h: i32, w: i32, cluster: cluster []}

  let grab_mouse = false

  let init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_square 10 20
    let cluster = cluster with basis.position.x = cluster.basis.position.x + 900
                          with basis.position.y = cluster.basis.position.y + 300
    in {time=0, h=i32.i64 h, w=i32.i64 w, cluster}

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = i32.i64 h with w = i32.i64 w

  let event (e: event) (s: state): state =
    match e
    case #step td ->
      let changes = [ (s.cluster.particles[20*10-1], {x=0.5, y=0})
                    , (s.cluster.particles[0], {x= -0.5, y=0})
                    , (s.cluster.particles[20-1], {x=0, y= -0.5})
                    , (s.cluster.particles[20*9], {x=0, y=0.5})
                    ]
      in s with time = s.time + td
           with cluster.basis = adjust_basis changes s.cluster.basis
    case _ -> s

  let render (s: state): [][]argb.colour =
    let {x=x0, y=y0} = s.cluster.basis.position
    let a0 = s.cluster.basis.orientation
    let background = tabulate_2d (i64.i32 s.h) (i64.i32 s.w) (const (const argb.black))

    -- triangle test
    let t_slopes = triangle_slopes (normalize_triangle_points ({x=50, y=50}, {x=150, y=75}, {x=110, y=200}))
    let ts_is = map (\({x, y}, _) -> (i64.i32 x, i64.i32 y)) (triangle_points [t_slopes])
    let background = scatter_2d background ts_is (map (const argb.green) ts_is)

    let mk_particle_is p =
      let a = a0 + p.basis_angle
      let y0 = i64.f32 (y0 + f32.sin a * p.basis_distance)
      let x0 = i64.f32 (x0 + f32.cos a * p.basis_distance)
      in map (\(y, x) -> (y0 + y, x0 + x)) circle_points
    let particles_is = map mk_particle_is s.cluster.particles |> flatten
    in scatter_2d background particles_is (map (const argb.white) particles_is)

  type text_content = text_content

  let text_format () = "FPS: %d\nAbsolute orientation: %.03f (sign: %.01f)"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, f32.abs s.cluster.basis.orientation, f32.sgn s.cluster.basis.orientation)

  let text_colour = const argb.green
}
