import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "scanline"

module vec2 = mk_vspace_2d f32

type position = vec2.vector

type particle = {basis_distance: f32, basis_angle: f32}

type basis = {position: position, orientation: f32}

type cluster [n] = {basis: basis, particles: [n]particle}

let pos_of_angle (basis_orientation: f32) (particle: particle): position =
  let a = particle.basis_angle + basis_orientation
  in {x=particle.basis_distance * f32.cos a,
      y=particle.basis_distance * f32.sin a}

def adjust_basis [n] (changes: [n](particle, position)) (basis: basis): basis =
  let calc (particle, position_change) =
    let {x, y} = pos_of_angle basis.orientation particle
    let (x', y') = (x + position_change.x, y + position_change.y)
    let basis_angle' = f32.atan2 y' x' - basis.orientation
    let angle_diff = basis_angle' - particle.basis_angle
    in if f32.abs angle_diff > 1 then 0 else angle_diff -- hack, fixme

  let angle_diffs = map calc changes
  in basis with orientation = basis.orientation + f32.sum angle_diffs
           with position.x = basis.position.x + f32.sum (map (.1.x) changes)
           with position.y = basis.position.y + f32.sum (map (.1.y) changes)

def mk_triangle (t0: position) (t1: position) (t2: position): cluster [] =
  let basis = {position={x=(t0.x + t1.x + t2.x) / 3, y=(t0.y + t1.y + t2.y) / 3}, orientation=0}
  let to_particle (p: position): particle =
    let x = p.x - basis.position.x
    let y = p.y - basis.position.y
    in {basis_distance=f32.sqrt (x * x + y * y), basis_angle=f32.atan2 y x}
  let particles = [to_particle t0, to_particle t1, to_particle t2]
  in {basis, particles}

def triangle_points [n] (triangle_slopes: [n]triangle_slopes) =
  let lines = lines_of_triangles triangle_slopes (replicate n ())
  in points_of_lines lines

type text_content = (i32, f32, f32)
module lys: lys with text_content = text_content = {
  type~ state = {time: f32, h: i32, w: i32, cluster: cluster []}

  let grab_mouse = false

  let init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_triangle {x=50, y=50} {x=150, y=75} {x=110, y=200}
    let cluster = cluster with basis.position.x = cluster.basis.position.x + 400
                          with basis.position.y = cluster.basis.position.y + 800
    in {time=0, h=i32.i64 h, w=i32.i64 w, cluster}

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = i32.i64 h with w = i32.i64 w

  let event (e: event) (s: state): state =
    match e
    case #step td ->
      let changes = [ (s.cluster.particles[0], {x= 0, y= -0.05})
                    , (s.cluster.particles[2], {x= 0, y= -1.0})
                    ]
      in s with time = s.time + td
           with cluster.basis = adjust_basis changes s.cluster.basis
    case _ -> s

  let render (s: state): [][]argb.colour =
    let to_point (p: particle): point =
      let {x, y} = pos_of_angle s.cluster.basis.orientation p
      in {x=t32 (f32.round (x + s.cluster.basis.position.x)),
          y=t32 (f32.round (y + s.cluster.basis.position.y))}
    let t_slopes = triangle_slopes (normalize_triangle_points (to_point s.cluster.particles[0],
                                                               to_point s.cluster.particles[1],
                                                               to_point s.cluster.particles[2]))
    let ts_is = map (\({x, y}, _) -> (i64.i32 y, i64.i32 x)) (triangle_points [t_slopes])
    let background = tabulate_2d (i64.i32 s.h) (i64.i32 s.w) (const (const argb.black))
    in scatter_2d background ts_is (map (const argb.green) ts_is)

  type text_content = text_content

  let text_format () = "FPS: %d\nAbsolute orientation: %.03f (sign: %.01f)"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, f32.abs s.cluster.basis.orientation, f32.sgn s.cluster.basis.orientation)

  let text_colour = const argb.green
}
