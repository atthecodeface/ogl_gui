val gl_int_val :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int
val gl_with_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int -> 'a
val read_file : string -> (string, 'a) result
module Gl_program :
  sig
    type t = {
      prog_id : int;
      attrib_ids : (string * int) list;
      uniform_ids : (string * int) list;
    }
    type desc = {
      tess_control_src : string option;
      tess_evaluation_src : string option;
      vertex_src : string;
      fragment_src : string;
      attribs : string list;
      uniforms : string list;
    }
    val make_desc : ?tess_control_src:string -> ?tess_evaluation_src:string -> string -> string -> string list -> string list -> desc
    val make : desc -> (t, string) result
    val delete : t -> (unit, 'a) result
    val get_attrib_id : ((string * int) list * 'a) list -> t -> 'a
    val get_uniform_id : string -> t -> int
  end
