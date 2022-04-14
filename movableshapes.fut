import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"
module scanline = import "scanline"

module real = f32
type real = real.t
module vec2 = mk_vspace_2d real
type vector = vec2.vector

type vector_generic 'a = {y: a, x: a}

type triangle 'a = (a, a, a)

type particle = {basis_distance: real, basis_angle: real, velocity: vector}

type basis = {position: vector, orientation: real}

type cluster [n] = {basis: basis, particles: [n]particle}

def particle_pos_rel (basis: basis) (particle: particle): vector =
  let a = particle.basis_angle + basis.orientation
  in vec2.scale particle.basis_distance {y=real.sin a, x=real.cos a}

def particle_pos_abs (basis: basis) (particle: particle): vector =
  vec2.(basis.position + particle_pos_rel basis particle)

def vec2_sum: []vector -> vector =
  reduce_comm (vec2.+) vec2.zero

def vec2_map_generic 'a 'b (f: a -> b) (v: vector_generic a): vector_generic b =
  {y=f v.y, x=f v.x}

def vec2_to_tuple 'a ({y, x}: vector_generic a): (a, a) =
  (y, x)

def adjust_basis [n] (basis: basis) (updated_particles: [n]particle): basis =
  let calc_angle_diff (particle: particle): real =
    let {x, y} = vec2.(particle_pos_rel basis particle + particle.velocity)
    let basis_angle' = real.atan2 y x - basis.orientation
    let angle_diff = basis_angle' - particle.basis_angle
    in if real.abs angle_diff > 1 then 0 else angle_diff -- FIXME (1 is arbitrary)

  let angle_diffs = map calc_angle_diff updated_particles
  in basis with orientation = basis.orientation + real.sum angle_diffs
           with position = basis.position vec2.+ vec2_sum (map (.velocity) updated_particles)

def mk_triangle (t0: vector) (t1: vector) (t2: vector): cluster [] =
  let basis = {position={y=(t0.y + t1.y + t2.y) / 3,
                         x=(t0.x + t1.x + t2.x) / 3},
               orientation=0}
  let to_particle (p: vector): particle =
    let v = vec2.(p - basis.position)
    in {basis_distance=vec2.norm v, basis_angle=real.atan2 v.y v.x, velocity=vec2.zero}
  let particles = [to_particle t0, to_particle t1, to_particle t2]
  in {basis, particles}

def triangle_points [n] (triangles: [n](triangle (vector_generic i32))): [](vector_generic i32) =
  let slopes = map (scanline.normalize_triangle_points >-> scanline.triangle_slopes) triangles
  let aux = replicate n () -- We don't use this feature for now.
  let lines = scanline.lines_of_triangles slopes aux
  let (points, _aux) = unzip (scanline.points_of_lines lines)
  in points

let planet: vector = {y=900, x=1000}
let planet_mass = 15f32

type text_content = (i32, real, real, real)
module lys: lys with text_content = text_content = {
  type~ state = {time: real, h: i32, w: i32, cluster: cluster []}

  let grab_mouse = false

  let init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_triangle {y=50, x=50} {y=75, x=150} {y=200, x=110}
    let cluster = cluster with basis.position = vec2.(cluster.basis.position + {y=400, x=400})
    in {time=0, h=i32.i64 h, w=i32.i64 w, cluster}

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = i32.i64 h with w = i32.i64 w

  let event (e: event) (s: state): state =
    match e
    case #step td ->
      let update_particle (p: particle): particle =
        let v = vec2.(planet - particle_pos_abs s.cluster.basis p)
        let attraction = planet_mass / (vec2.norm v)**2
        let friction = 0.99
        let p' = p with velocity = vec2.(scale friction p.velocity + scale attraction v)
        in p'

      let particles' = map update_particle s.cluster.particles
      in s with time = s.time + td
           with cluster.basis = adjust_basis s.cluster.basis particles'
           with cluster.particles = particles'
    case _ -> s

  let render (s: state): [][]argb.colour =
    let render_planet [m][n] (background: *[m][n]argb.colour): *[m][n]argb.colour =
      let radius = 20f32
      let diameter = radius * 2
      let planet_coor =
        let is_in_circle pos = vec2.norm pos < radius
        let diameter' = i64.f32 diameter
        in tabulate_2d diameter' diameter'
                       (\y x ->
                          vec2_map_generic real.i64 {y, x}
                          |> vec2.map (\k -> k - radius))
           |> flatten
           |> filter is_in_circle
           |> map ((vec2.+ planet) >-> vec2_map_generic i64.f32 >-> vec2_to_tuple)
      in scatter_2d background planet_coor (map (const argb.white) planet_coor)

    let render_cluster [m][n] (background: *[m][n]argb.colour): *[m][n]argb.colour =
      let to_point: particle -> vector_generic i32 =
        particle_pos_abs s.cluster.basis >-> vec2_map_generic (real.round >-> t32)
      let triangles = [(to_point s.cluster.particles[0],
                        to_point s.cluster.particles[1],
                        to_point s.cluster.particles[2])]
      let triangles_coor = map (vec2_map_generic i64.i32 >-> vec2_to_tuple) (triangle_points triangles)
      in scatter_2d background triangles_coor (map (const argb.green) triangles_coor)

    let background = replicate (i64.i32 s.h) (replicate (i64.i32 s.w) argb.black)
    let background = render_planet background
    let background = render_cluster background
    in background

  type text_content = text_content

  let text_format () =
    "FPS: %d\n"
    ++ "Cluster position: {y=%.03f, x=%.03f}\n"
    ++ "Cluster orientation: %.03f°"

  let text_content (render_duration: real) (s: state): text_content =
    (t32 render_duration,
     s.cluster.basis.position.y, s.cluster.basis.position.x,
     s.cluster.basis.orientation * 180 / real.pi)

  let text_colour = const argb.green
}
