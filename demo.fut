import "lib/github.com/diku-dk/lys/lys"
import "movableshapes"

type text_content = (i32, real, real, real)
module lys: lys with text_content = text_content = {
  type~ state = {time: real,
                 h: i64, w: i64,
                 cluster: cluster,
                 planet: planet}

  def grab_mouse = false

  def init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_triangles
                  -- [{y=50, x=50}, {y=75, x=150}, {y=200, x=110}, {y=270, x=200}]
                  [{y=100, x=50}, {y=50, x=50}, {y=100, x=800}, {y=50, x=800}]
                  [(0, 1, 2), (1, 2, 3)]
    let planet = {position={y=real.i64 h / 2, x=real.i64 w / 2},
                  radius=20,
                  mass=10f32}
    in {time=0, h, w, cluster, planet}

  def resize (h: i64) (w: i64) (s: state): state =
    s with h = h with w = w

  def step (td: f32) (s: state): state =
    let particles' = map (update_particle_for_planet s.planet s.cluster.basis) s.cluster.particles
    in s with time = s.time + td
         with cluster.basis = adjust_basis s.cluster.basis particles'
         with cluster.particles = particles'

  def mouse ((x, y): (i32, i32)) (s: state): state =
    s with planet.position = {y=r32 y, x=r32 x}

  def event (e: event) (s: state): state =
    match e
    case #step td -> step td s
    case #mouse {buttons=_, x, y} -> mouse (x, y) s
    case _ -> s

  def render (s: state): [][]argb.colour =
    mk_background s.h s.w argb.black
    ||> render_planet s.planet
    ||> render_cluster s.cluster

  type text_content = text_content

  def text_format () =
    "FPS: %d\n"
    ++ "Cluster position: {y=%.03f, x=%.03f}\n"
    ++ "Cluster orientation: %.03f°"

  def text_content (render_duration: real) (s: state): text_content =
    (t32 render_duration,
     s.cluster.basis.position.y, s.cluster.basis.position.x,
     s.cluster.basis.orientation * 180 / real.pi)

  def text_colour = const argb.green
}
