module Animatable :
  sig
    type set = { mutable elements : t list; mutable any_changing : bool; }
    and t = {
      mutable target_time_ms : int;
      mutable last_time_ms : int;
      mutable changing : bool;
      mutable transition_fn : int -> int -> int -> bool;
      mutable opt_parent : set option;
    }
    val create : 'a -> t
    val set_recalculate : set -> unit
    val set_transition : (int -> int -> int -> bool) -> t -> unit
    val time_step : int -> t -> unit
    val set_target : int -> int -> t -> unit
    val is_changing : t -> bool
    val set_create : t list -> set
    val set_any_changing : set -> bool
    val add_element : t -> set -> unit
  end
module Animatable_linear_float :
  sig
    type t = {
      anim : Animatable.t;
      current_value : float array;
      target_value : float array;
    }
    val set_value : t -> float array -> unit
    val get_value : t -> float array
    val transition_fn : t -> int -> int -> int -> bool
    val create : float array -> t
    val is_changing : t -> bool
    val time_step : int -> t -> unit
    val set_target : int -> int -> float array -> t -> unit
  end
