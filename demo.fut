import "lib/github.com/diku-dk/lys/lys"
import "movableshapes"

type text_content = (i32, real, real, real)
module lys: lys with text_content = text_content = {
  type~ state = {time: real,
                 h: i64, w: i64,
                 cluster: cluster,
                 planet: planet}

  let grab_mouse = false

  let init (_seed: u32) (h: i64) (w: i64): state =
    let cluster = mk_triangles
                  [{y=50, x=50}, {y=75, x=150}, {y=200, x=110}, {y=270, x=200}]
                  [(0, 1, 2), (1, 2, 3)]
    let planet = {position={y=900, x=1000},
                  radius=20,
                  mass=15f32}
    in {time=0, h, w, cluster, planet}

  let resize (h: i64) (w: i64) (s: state): state =
    s with h = h with w = w

  let event (e: event) (s: state): state =
    match e
    case #step td ->
      let particles' = map (update_particle_for_planet s.planet s.cluster.basis) s.cluster.particles
      in s with time = s.time + td
           with cluster.basis = adjust_basis s.cluster.basis particles'
           with cluster.particles = particles'
    case _ -> s

  let render (s: state): [][]argb.colour =
    mk_background s.h s.w argb.black
    ||> render_planet s.planet
    ||> render_cluster s.cluster

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
