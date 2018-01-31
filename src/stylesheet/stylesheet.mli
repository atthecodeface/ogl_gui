val trace : string * int * int * int -> unit
val sfmt : ('a, unit, string) format -> 'a
val option_is_none : 'a option -> bool
val option_is_some : 'a option -> bool
val option_get : 'a option -> 'a
type t_stylable_name = string
type t_stylable_value =
    Sv_float_6 of float array
  | Sv_float_3 of float array
  | Sv_float of float
  | Sv_int of int
  | Sv_int_3 of int array
  | Sv_int_6 of int array
  | Sv_rgb of float array
val sv_zero : t_stylable_value
val sv_none : t_stylable_value
type t_stylable_type =
    St_float_6
  | St_float_3
  | St_float
  | St_int
  | St_int_3
  | St_int_6
  | St_rgb
val stype_of_svalue : t_stylable_value -> t_stylable_type
val str_of_svalue : t_stylable_value -> string
val string_as_float_rex : Re.re
val string_as_int_rex : Re.re
val extract_first_and_rest : Re.re -> string -> (string * string) option
val fill_array : 'a array -> int -> int -> int -> int -> 'a array
val read_floats : string -> int -> float array
val read_ints : string -> int -> int array
val svalue_from_string : t_stylable_type -> string -> t_stylable_value
val one_i_0 : int array
val one_f_0 : float array
val svalue_as_floats : t_stylable_value -> float array
val svalue_as_float : t_stylable_value -> float
val svalue_as_ints : t_stylable_value -> int array
module Style_id_hash : sig type t = string val of_string : 'a -> 'a end
module Style_id :
  sig
    type t = {
      hash : Style_id_hash.t;
      name : string;
      stype : t_stylable_type;
    }
    val dummy : t
    val create : Style_id_hash.t -> t_stylable_type -> t
    val get_type : t -> t_stylable_type
    val str : t -> string
  end
module Style_ids :
  sig
    exception Unknown_id of string
    exception Duplicate_id
    type t = { set : (Style_id_hash.t, Style_id.t) Hashtbl.t; }
    val create : 'a -> t
    val find_opt_id : Style_id_hash.t -> t -> Style_id.t option
    val find_id_exn : Style_id_hash.t -> t -> Style_id.t
    val add_id : Style_id_hash.t -> Style_id.t -> t -> unit
    val build_id_value_list :
      (Style_id_hash.t * 'a) list -> t -> (Style_id.t * 'a) list
  end
module Style :
  sig
    type t = {
      mutable styles : (Style_id.t * (t_stylable_value * bool)) list;
    }
    val create : (Style_id.t * (t_stylable_value * bool)) list -> t
    val add_styling : Style_id.t -> t_stylable_value -> bool -> t -> unit
    val str : t -> string
    val get_value : Style_id.t -> t -> t_stylable_value
    val get_opt : Style_id.t -> t -> bool
  end
module Stylable_desc :
  sig
    type t = {
      state_descriptor : (string * (string * int) list) list;
      styles : (string * t_stylable_type) list;
    }
    val create :
      (string * (string * int) list) list ->
      (string * t_stylable_type) list -> t
  end
module Stylable_desc_built :
  sig
    type t = { desc : Stylable_desc.t; sids : Style_id.t array; }
    val get_nth_sid : int -> t -> Style_id.t
    exception Style_id_not_found_in_binding of string
    exception Style_type_mismatch of string
    val create : Stylable_desc.t -> Style_ids.t -> t
    val find_sid_index : Style_id.t -> t -> int option
    val find_sid_index_exn : Style_id.t -> t -> int
  end
module Stylable_value_ref :
  sig
    type t_stylable_value_ref = Ref of t_stylable_value | Default | Inherit
    type t = {
      mutable longest_rule : int;
      mutable next_value_ref : t_stylable_value_ref;
      mutable value : t_stylable_value;
      mutable default_value : t_stylable_value;
      mutable default_inherit : bool;
    }
    val create : 'a -> t
    val get_value : t -> t_stylable_value
    val get_value_as_floats : t -> float array
    val get_value_as_float : t -> float
    val get_value_as_ints : t -> int array
    val set_default_inherit : t -> bool -> unit
    val set_value : t -> t_stylable_value -> unit
    val set_default_from_string : t -> t_stylable_type -> string -> unit
    val reset : t -> unit
    val next_value_ref : t -> t_stylable_value_ref
    val apply : int -> t_stylable_value -> t -> unit
  end
val svr_zero : Stylable_value_ref.t
module rec Stylable :
  sig
    type t_style_selector = t -> bool
    and t_style_change_callback =
        (Style_id.t * t_stylable_value) list -> unit
    and t_style_change_fn = t -> unit
    and t = {
      sheet : Stylesheet.t;
      desc_built : Stylable_desc_built.t;
      num_base_styles : int;
      num_styles : int;
      extra_sids : Style_id.t array;
      children : t list;
      mutable parent : t option;
      state : int array;
      style_change_callback : t_style_change_callback;
      id_name : string;
      type_name : string;
      classes : string list;
      values : Stylable_value_ref.t array;
    }
    val create :
      Stylable_desc.t ->
      Stylesheet.t ->
      string ->
      (string * string) list -> t_style_change_callback -> t list -> t
    val set_parent : t -> t -> unit
    val set_element_state : int -> int -> t -> unit
    val get_id : t -> string
    val get_type : t -> string
    val is_element_id : string -> t -> bool
    val is_element_type : string -> t -> bool
    val has_element_class : string -> t -> bool
    val is_element_state : int -> int -> t -> bool
    val get_value_ref : t -> string -> Stylable_value_ref.t
    val get_value : t -> string -> t_stylable_value
    val reset_next_values : t -> unit
    val apply_styles :
      int -> (Style_id.t * t_stylable_value) list -> t -> unit
    val update_current_values_from_next : t -> unit
    val element_callback_matching_children :
      t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit
    val element_callback_matching_subelements :
      t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit
    val element_callback_matching_tree :
      t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit
  end
and Style_rule :
  sig
    type t = {
      selectors : Stylable.t_style_selector list;
      styles : (Style_id.t * t_stylable_value) list;
    }
    val create :
      Stylable.t_style_selector list ->
      (Style_id.t * t_stylable_value) list -> t
    val apply : t -> Stylesheet.t -> unit
  end
and Stylesheet :
  sig
    type t = {
      mutable entity_list : Stylable.t list;
      mutable roots : Stylable.t list;
      ids : Style_ids.t;
      default_style : Style.t;
      mutable rules : Style_rule.t list;
      mutable built_descs : (Stylable_desc.t * Stylable_desc_built.t) list;
    }
    val create : unit -> t
    val build : t -> Stylable.t list -> t
    val add_stylable : Stylable.t -> t -> unit
    val add_style_defaults :
      t -> (string * t_stylable_value * bool) list -> unit
    val element_callback_matching_tree :
      t -> Stylable.t_style_selector -> Stylable.t_style_change_fn -> unit
    val add_style_rule :
      t ->
      Stylable.t_style_selector list ->
      (t_stylable_name * t_stylable_value) list -> unit
    val apply_stylesheet : t -> unit
    val get_default_value : Style_id.t -> t -> t_stylable_value
    val is_default_inherit : Style_id.t -> t -> bool
    val style_id_of_name : string -> t -> Style_id.t option
    val style_id_of_name_exn : string -> t -> Style_id.t
    val build_desc : Stylable_desc.t -> t -> Stylable_desc_built.t
  end
