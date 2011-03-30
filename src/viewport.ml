module rec Sizes =
struct
  type size =
    | Absolute of float
    | Rel_not_updated of float
    | Rel_updated of float * float

  type t = {
    mutable line_width: size;
    mutable text_size: size;
    mutable mark_size: size
  }
end
and Axes =
struct
  type labels =
    | Text of string array * float
    | Number
    | Expnumber of float
    | Expnumber_named of float * string
    | Custom of (float -> string)

  type tics =
    | Fixed of float list
    | Fixed_norm of float list
    | Equidistants of int * int
    | Auto

  type sign = Positive | Negative;

  type offset =
    | Relative of float
    | Absolute of float

  type axis = {
    tics: tics;
    offset: float;
    tics_position: sign
  }

  type dim = {
    mutable x0: float;
    mutable xend: float;
    mutable log: bool;
    mutable orientation: sign;
    mutable axes: axis list
  }

  type t = {
    dimx: dim;
    dimy: dim;

    mutable viewports: Viewport.t list
  }
end
and Viewport = struct
  type t = {
    backend: Backend.t;

    parent: t;
    mutable children: t list;

    (* (A,B,C,E) indicate "1" for a particulate (see below) coordinate system
       A---------device--------+
       | B--------graph------+ |
       | C         Data      | |
       | +-C-E---------------B |
       +-----------------------A
    *)
    mutable coord_device: Coordinate.t; (* AA *)
    mutable coord_graph: Coordinate.t; (* BB *)
    mutable coord_data: Coordinate.t; (* CC *)
    mutable coord_normalized: Coordinate.t; (* BE *)

    (* Axes system associated to the viewport *)
    mutable axes_system: Axes.t;
    (* For sizing texts, tics, etc. *)
    mutable sizes: Sizes.t;
    (* The last point drawn *)
    mutable current_point: float * float;
    (* An instruction is a "thing" to plot on the device, we memorize
       their order to replot in case of necessity *)
    mutable instructions: (unit -> unit) Queue.t;
    
    (* Draw immediately or wait for closing ? *)
    mutable immediate_drawing: bool
  }

  let make_root backend =
    let size0 = min backend.width backend.height in
    let dim = {
      Axis.x0 = neg_infinity;
      Axis.xend = infinity;
      Axis.log = false;
      Axis.orientation = Axis.Positive;
      Axis.axes = []
    } in
    let axes_system = {
      dimx = dim;
      dimy = {dim with Axis.x0 = dim.Axis.x0};
      viewports = []
    } in
    let rec real_root = {
      backend = backend;
      parent = real_root;
      children = [];
      
      (* TODO Complete that *)
      coord_device = ...;
      coord_graph = ...;
      coord_data = ...;
      coord_normalized = ...;

      axes_system = axes_system;

      sizes = {
	Sizes.line_width = Sizes.Absolute size0;
	Sizes.text_size = Sizes.Absolute size0;
	Sizes.mark_size = Sizes.Absolute 42. (* TODO Check this *)
      };
      current_point = (0., 0.);
      instructions = [];
      immediate_drawing = false;
    }
end
