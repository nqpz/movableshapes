import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

module vec2 = mk_vspace_2d f32

type position = vec2.vector

type particle = {basis_distance: f32, basis_angle: f32}

type basis = {position: position, orientation: f32}

type cluster [n] = {basis: basis, particles: [n]particle}

def radius = 10i64
def diameter = radius * 2

def adjust_basis (particle: particle) (position_change: position) (basis: basis): basis =
  let pos_of_angle basis_angle =
    let a = basis.orientation + basis_angle
    in {x=particle.basis_distance * f32.cos a,
        y = particle.basis_distance * f32.sin a}
  let {x, y} = pos_of_angle particle.basis_angle
  let basis_angle' = f32.atan2 (y + position_change.y) (x + position_change.x) - basis.orientation
  let angle_diff = basis_angle' - particle.basis_angle
  -- let {x=x1, y=y1} = pos_of_angle basis_angle'
  -- let (x_diff, y_diff) = (x1 - x, y1 - y)
  in basis with orientation = basis.orientation + angle_diff
           with position.x = basis.position.x + position_change.x -- - x_diff
           with position.y = basis.position.y + position_change.y -- - y_diff

  -- basis
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

type text_content = i32
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
      let cluster = s.cluster
      let cluster = cluster with basis = adjust_basis s.cluster.particles[20*10-1] {x=0.5, y=0} cluster.basis
--      let cluster = cluster with basis = adjust_basis s.cluster.particles[20*9] {x= -0.5, y=0} cluster.basis
      let cluster = cluster with basis = adjust_basis s.cluster.particles[0] {x= -0.5, y=0} cluster.basis

      let cluster = cluster with basis = adjust_basis s.cluster.particles[20-1] {x=0, y= -0.5} cluster.basis
      let cluster = cluster with basis = adjust_basis s.cluster.particles[20*9] {x=0, y=0.5} cluster.basis
      in s with time = s.time + td
           with cluster = cluster
    case _ -> s

  let render (s: state): [][]argb.colour =
    let {x=x0, y=y0} = s.cluster.basis.position
    let a0 = s.cluster.basis.orientation
    let background = tabulate_2d (i64.i32 s.h) (i64.i32 s.w) (const (const argb.black))
    let mk_particle_is p =
      let a = a0 + p.basis_angle
      let y0 = i64.f32 (y0 + f32.sin a * p.basis_distance)
      let x0 = i64.f32 (x0 + f32.cos a * p.basis_distance)
      in map (\(y, x) -> (y0 + y, x0 + x)) circle_points
    let particles_is = map mk_particle_is s.cluster.particles |> flatten
    in scatter_2d background particles_is (map (const argb.white) particles_is)

  type text_content = text_content

  let text_format () = "FPS: %d"

  let text_content (render_duration: f32) (_: state): text_content =
    t32 render_duration

  let text_colour = const argb.green
}
