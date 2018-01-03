val gl_int_val :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int
val gl_with_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int -> 'a
module Gl_program :
  sig
    val reset_shader_path : unit -> unit
    val add_shader_path : string -> unit
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
module Material :
sig
  type t = {
      prog_id    : int;
      p_uid      : int;
      g_uid      : int;
      other_uids : int array;
    }
  val create : Gl_program.t -> string array -> t Utils.ogl_result
  val set_projection     : t -> Utils.float32_bigarray -> Utils.float32_bigarray -> int array
  val set_transformation : t -> Utils.float32_bigarray -> int array
end
