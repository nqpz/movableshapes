import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"
module scanline = import "scanline"

module real = f32
type real = real.t
module vec2 = mk_vspace_2d real
type vector = vec2.vector

type vector_generic 'a = {y: a, x: a}

type triangle 'a = (vector_generic a, vector_generic a, vector_generic a)

type planet = {position: vector, radius: real, mass: real} -- FIXME: Calculate depending on circle area

type particle = {basis_distance: real, basis_angle: real, velocity: vector}

type basis = {position: vector, orientation: real}

type~ cluster = {basis: basis, particles: []particle, triangles: [](i32, i32, i32)}

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

def mk_triangle ((t0, t1, t2): triangle real): cluster =
  let basis = {position={y=(t0.y + t1.y + t2.y) / 3,
                         x=(t0.x + t1.x + t2.x) / 3},
               orientation=0}
  let to_particle (p: vector): particle =
    let v = vec2.(p - basis.position)
    in {basis_distance=vec2.norm v, basis_angle=real.atan2 v.y v.x, velocity=vec2.zero}
  let particles = [to_particle t0, to_particle t1, to_particle t2]
  let triangles = [(0, 1, 2)]
  in {basis, particles, triangles}

def triangle_points [n] (triangles: [n](triangle i32)): [](vector_generic i32) =
  let slopes = map (scanline.normalize_triangle_points >-> scanline.triangle_slopes) triangles
  let aux = replicate n () -- We don't use this feature for now.
  let lines = scanline.lines_of_triangles slopes aux
  let (points, _aux) = unzip (scanline.points_of_lines lines)
  in points

def update_particle_for_planet (planet: planet) (basis: basis) (p: particle): particle =
  let v = vec2.(planet.position - particle_pos_abs basis p)
  let attraction = planet.mass / (vec2.norm v)**2
  let friction = 0.99
  in p with velocity = vec2.(scale friction p.velocity + scale attraction v)

def render_planet [m][n] (planet: planet) (background: *[m][n]argb.colour): *[m][n]argb.colour =
  let diameter = planet.radius * 2
  let planet_coor = -- FIXME: Draw the circle with scanline instead of filter
    let is_in_circle pos = vec2.norm pos < planet.radius
    let diameter' = i64.f32 diameter
    in tabulate_2d diameter' diameter'
                   (\y x ->
                      vec2_map_generic real.i64 {y, x}
                      |> vec2.map (\k -> k - planet.radius))
       |> flatten
       |> filter is_in_circle
       |> map ((vec2.+ planet.position) >-> vec2_map_generic i64.f32 >-> vec2_to_tuple)
  in scatter_2d background planet_coor (map (const argb.white) planet_coor)

def render_cluster [m][n] (cluster: cluster) (background: *[m][n]argb.colour): *[m][n]argb.colour =
  let triangle_color = argb.green -- FIXME: Maybe don't hardcode this.
  let to_point: particle -> vector_generic i32 =
    particle_pos_abs cluster.basis >-> vec2_map_generic (real.round >-> t32)
  let triangles = map (\(i0, i1, i2) ->
                         #[unsafe] (to_point cluster.particles[i0],
                                    to_point cluster.particles[i1],
                                    to_point cluster.particles[i2]))
                      cluster.triangles
  let triangles_coor = map (vec2_map_generic i64.i32 >-> vec2_to_tuple) (triangle_points triangles)
  in scatter_2d background triangles_coor (map (const triangle_color) triangles_coor)

def mk_background (h: i64) (w: i64) (color: argb.colour): [h][w]argb.colour =
  replicate h (replicate w color)

def (||>) [m][n] 'a (x: *[m][n]a) (f: *[m][n]a -> *[m][n]a): *[m][n]a =
  f x
