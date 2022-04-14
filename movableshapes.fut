import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "scanline"

module vec2 = mk_vspace_2d f32

type particle = {basis_distance: f32, basis_angle: f32, velocity: vec2.vector}

type basis = {position: vec2.vector, orientation: f32}

type cluster [n] = {basis: basis, particles: [n]particle}

let pos_of_angle (basis_orientation: f32) (particle: particle): vec2.vector =
  let a = particle.basis_angle + basis_orientation
  in {x=particle.basis_distance * f32.cos a,
      y=particle.basis_distance * f32.sin a}

def adjust_basis [n] (changes: [n](particle, vec2.vector)) (basis: basis): basis =
  let calc (particle, position_change) =
    let {x, y} = pos_of_angle basis.orientation particle
    let (x', y') = (x + position_change.x, y + position_change.y)
    let basis_angle' = f32.atan2 y' x' - basis.orientation
    let angle_diff = basis_angle' - particle.basis_angle
    in if f32.abs angle_diff > 1 then 0 else angle_diff -- FIXME

  let angle_diffs = map calc changes
  in basis with orientation = basis.orientation + f32.sum angle_diffs
           with position.x = basis.position.x + f32.sum (map (.1.x) changes)
           with position.y = basis.position.y + f32.sum (map (.1.y) changes)

def mk_triangle (t0: vec2.vector) (t1: vec2.vector) (t2: vec2.vector): cluster [] =
  let basis = {position={x=(t0.x + t1.x + t2.x) / 3, y=(t0.y + t1.y + t2.y) / 3}, orientation=0}
  let to_particle (p: vec2.vector): particle =
    let v = vec2.(p - basis.position)
    in {basis_distance=vec2.norm v, basis_angle=f32.atan2 v.y v.x, velocity=vec2.zero}
  let particles = [to_particle t0, to_particle t1, to_particle t2]
  in {basis, particles}

def triangle_points [n] (triangle_slopes: [n]triangle_slopes) =
  let lines = lines_of_triangles triangle_slopes (replicate n ())
  in points_of_lines lines

let planet: vec2.vector = {x=1000, y=900}
let planet_mass = 15f32

type text_content = (i32, f32, f32, f32)
module lys: lys with text_content = text_content = {
  type~ state = {time: f32, h: i32, w: i32, cluster: cluster []}

  let grab_mouse = false

  let init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_triangle {x=50, y=50} {x=150, y=75} {x=110, y=200}
    let cluster = cluster with basis.position.x = cluster.basis.position.x + 400
                          with basis.position.y = cluster.basis.position.y + 400
    in {time=0, h=i32.i64 h, w=i32.i64 w, cluster}

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = i32.i64 h with w = i32.i64 w

  let event (e: event) (s: state): state =
    match e
    case #step td ->

      let particle_change (p: particle): (particle, vec2.vector) =
        let pp = pos_of_angle s.cluster.basis.orientation p
        let v = vec2.(planet - (pp + s.cluster.basis.position))
        let dist = vec2.norm v
        let f = planet_mass / dist**2
        let friction = 0.99
        let p = p with velocity = vec2.(scale friction p.velocity + scale f v)
        in (p, p.velocity)

      let changes = map particle_change s.cluster.particles
      in s with time = s.time + td
           with cluster.basis = adjust_basis changes s.cluster.basis
           with cluster.particles = map (.0) changes
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

    let radius = 20
    let diameter = radius * 2
    let planet_is =
      let is_in_circle (y, x) =
        let pos = {y=f32.i64 (y - radius), x=f32.i64 (x - radius)}
        in vec2.norm pos < f32.i64 radius
      in tabulate_2d diameter diameter (\y x -> (y, x))
         |> flatten
         |> filter is_in_circle
         |> map (\(y, x) -> (y + i64.f32 planet.y - radius, x + i64.f32 planet.x - radius))
    let background = scatter_2d background planet_is (map (const argb.white) planet_is)

    in scatter_2d background ts_is (map (const argb.green) ts_is)

  type text_content = text_content

  let text_format () = "FPS: %d\nCluster: (%.03f, %.03f) orientation %.03f"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, s.cluster.basis.position.x, s.cluster.basis.position.y, s.cluster.basis.orientation)

  let text_colour = const argb.green
}
