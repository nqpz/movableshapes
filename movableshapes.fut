import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
import "scanline"

module vec2 = mk_vspace_2d f32

type particle = {basis_distance: f32, basis_angle: f32, velocity: vec2.vector}

type basis = {position: vec2.vector, orientation: f32}

type cluster [n] = {basis: basis, particles: [n]particle}

def particle_pos_rel (basis: basis) (particle: particle): vec2.vector =
  let a = particle.basis_angle + basis.orientation
  in {x=particle.basis_distance * f32.cos a,
      y=particle.basis_distance * f32.sin a}

def particle_pos_abs (basis: basis) (particle: particle): vec2.vector =
  vec2.(basis.position + particle_pos_rel basis particle)

def vec2_sum: []vec2.vector -> vec2.vector = reduce_comm (vec2.+) vec2.zero

def adjust_basis [n] (changes: [n](particle, vec2.vector)) (basis: basis): basis =
  let calc (particle, position_change) =
    let {x, y} = vec2.(particle_pos_rel basis particle + position_change)
    let basis_angle' = f32.atan2 y x - basis.orientation
    let angle_diff = basis_angle' - particle.basis_angle
    in if f32.abs angle_diff > 1 then 0 else angle_diff -- FIXME (1 is arbitrary)

  let angle_diffs = map calc changes
  in basis with orientation = basis.orientation + f32.sum angle_diffs
           with position = basis.position vec2.+ vec2_sum (map (.1) changes)

def mk_triangle (t0: vec2.vector) (t1: vec2.vector) (t2: vec2.vector): cluster [] =
  let basis = {position={x=(t0.x + t1.x + t2.x) / 3, y=(t0.y + t1.y + t2.y) / 3}, orientation=0}
  let to_particle (p: vec2.vector): particle =
    let v = vec2.(p - basis.position)
    in {basis_distance=vec2.norm v, basis_angle=f32.atan2 v.y v.x, velocity=vec2.zero}
  let particles = [to_particle t0, to_particle t1, to_particle t2]
  in {basis, particles}

def triangle_points [n] (triangle_slopes: [n]triangle_slopes): []point =
  let aux = replicate n () -- We don't use this feature for now.
  let lines = lines_of_triangles triangle_slopes aux
  let (points, _aux) = unzip (points_of_lines lines)
  in points

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
        let v = vec2.(planet - particle_pos_abs s.cluster.basis p)
        let attraction = planet_mass / (vec2.norm v)**2
        let friction = 0.99
        let p' = p with velocity = vec2.(scale friction p.velocity + scale attraction v)
        in (p', p'.velocity)

      let changes = map particle_change s.cluster.particles
      in s with time = s.time + td
           with cluster.basis = adjust_basis changes s.cluster.basis
           with cluster.particles = map (.0) changes
    case _ -> s

  let render (s: state): [][]argb.colour =
    let render_planet [m][n] (background: *[m][n]argb.colour): *[m][n]argb.colour =
      let radius = 20f32
      let diameter = radius * 2
      let planet_is =
        let is_in_circle pos = vec2.norm pos < radius
        let diameter' = i64.f32 diameter
        in tabulate_2d diameter' diameter' (\y x -> vec2.map (\k -> k - radius)
                                                             {y=f32.i64 y, x=f32.i64 x})
           |> flatten
           |> filter is_in_circle
           |> map (\pos -> vec2.(pos + planet))
           |> map (\pos -> (i64.f32 pos.y, i64.f32 pos.x))
      in scatter_2d background planet_is (map (const argb.white) planet_is)

    let render_cluster [m][n] (background: *[m][n]argb.colour): *[m][n]argb.colour =
      let to_point (p: particle): point =
        let {x, y} = particle_pos_abs s.cluster.basis p
        in {x=t32 (f32.round x),
            y=t32 (f32.round y)}
      let t_slopes = triangle_slopes (normalize_triangle_points (to_point s.cluster.particles[0],
                                                                 to_point s.cluster.particles[1],
                                                                 to_point s.cluster.particles[2]))
      let ts_is = map (\{x, y} -> (i64.i32 y, i64.i32 x)) (triangle_points [t_slopes])
      in scatter_2d background ts_is (map (const argb.green) ts_is)

    let background = replicate (i64.i32 s.h) (replicate (i64.i32 s.w) argb.black)
    let background = render_planet background
    let background = render_cluster background
    in background

  type text_content = text_content

  let text_format () = "FPS: %d\nCluster: (%.03f, %.03f) orientation %.03f"

  let text_content (render_duration: f32) (s: state): text_content =
    (t32 render_duration, s.cluster.basis.position.x, s.cluster.basis.position.y, s.cluster.basis.orientation)

  let text_colour = const argb.green
}
