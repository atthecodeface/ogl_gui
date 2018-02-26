module Span :
  sig
    type t_span_element = {
      mutable cl : (Ogl_types.t_ogl_widget * int) list;
      mutable base_start : float;
      mutable base_size : float;
      mutable draw_start : float;
      mutable draw_size : float;
      mutable weight_grow : float;
      mutable weight_shrink : float;
    }
    type t = {
      mutable element_list :
        (Ogl_types.t_ogl_widget * float * int * int) list;
      mutable num_elements : int;
      mutable min_g : int;
      mutable max_g : int;
      mutable num_cells : int;
      mutable span_array : t_span_element array option;
      mutable span_size : float;
      mutable default_weight_grow : float;
      mutable default_weight_shrink : float;
      mutable weights_grow : float list;
      mutable weights_shrink : float list;
      mutable total_weight_grow : float;
      mutable total_weight_shrink : float;
      mutable widget_sizes : (Ogl_types.t_ogl_widget * float ref) list;
    }
    val create : 'a -> t
    val set_weight : t -> bool -> bool -> float list -> unit
    val nv_callbacks : (string * (t -> string -> unit)) list
    val set_data : (string * string) list -> t -> unit
    val add_place : 'a -> 'b -> 'c -> 'd -> unit
    val add_grid : t -> Ogl_types.t_ogl_widget -> int -> int -> unit
    val build_span_array : t -> unit
    val set_widget_size : t -> Ogl_types.t_ogl_widget -> float -> unit
    val get_widget_size : t -> Ogl_types.t_ogl_widget -> float
    val calculate_base_size : t -> float
    val draw_size_span_array : t -> float -> unit
    val get_draw_size_offsets :
      t -> (Ogl_types.t_ogl_widget -> float -> float -> unit) -> unit
  end
