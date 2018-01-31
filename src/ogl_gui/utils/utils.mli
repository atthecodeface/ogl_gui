type 'a ogl_result = ('a, string) Result.result
type float32_bigarray = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
type uint16_bigarray = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type 'a t_dims3 = 'a array
type 'a t_dims6 = 'a array
val matrix_translate : float array -> Atcflib.Matrix.t
val sfmt : ('a, unit, string) format -> 'a
val str_fa : float array -> string
val str_ia : int array -> string
val calc_dims_pair :
  (float -> float -> float -> float) ->
  float array -> float array -> float array
val calc_dims1 : 'a array -> ('a -> 'b -> 'c) -> 'b array -> 'c array
val calc_dims2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
val calc_dims3 :
  ('a -> 'b -> 'c -> 'd) -> 'a array -> 'b array -> 'c array -> 'd array
val calc_dims4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a array -> 'b array -> 'c array -> 'd array -> 'e array
val nspaces : int -> 'a -> string
exception Display_error of string
val display_any_error :
  ('a, string) Result.result -> ('a, string) Result.result
val raise_any_error :
  ('a, string) Result.result -> ('a, string) Result.result
val option_is_none : 'a option -> bool
val option_is_some : 'a option -> bool
val option_get : 'a option -> 'a
val option_get_default : 'a option -> 'a -> 'a
val option_apply : ('a -> 'b) -> 'b -> 'a option -> 'b
val ( >>= ) :
  ('a, 'b) Result.result ->
  ('a -> ('c, 'b) Result.result) -> ('c, 'b) Result.result
val ba_floats :
  float array ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_uint8s :
  int array ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_string :
  int ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_int32s :
  int ->
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_int32_1 :
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_float_array :
  int -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_uint16_array :
  int ->
  (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_uint8_array :
  int ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
val gl_int_val :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int
val gl_with_int :
  ((int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> 'a) ->
  int -> 'a
val gl_create_buffer : ('a, 'b) Tgl4.Gl.bigarray -> int
val gl_delete_buffer : int -> unit
val find_value : ('a * 'b) list -> 'a -> 'b option
val trace : string * int * int * int -> unit
val scan_value :
  ('a, Scanf.Scanning.in_channel, 'b, 'c -> 'd, 'a -> 'e, 'e) format6 ->
  'c -> string -> 'd option
val skip_whitespace : string -> int -> int
val scan_float : string -> int -> (float * int) option
val scan_float_list : (float list -> 'a) -> string -> 'a
val scan_float3 : (float -> float -> float -> 'a) -> string -> 'a option
val scan_float6 :
  (float -> float -> float -> float -> float -> float -> 'a) ->
  string -> 'a option
val scan_int3 : (int -> int -> int -> 'a) -> string -> 'a option
val name_value_get : ('a * 'b) list -> 'a -> 'b option
val name_value_callbacks :
  ('a * 'b) list -> ('a * ('c -> 'b -> 'd)) list -> 'c -> bool
val identity4 :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
val ba_of_matrix4 :
  Atcflib.Matrix.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
module Collider_ray :
  sig
    type t = {
      origin_v : Atcflib.Vector.t;
      direction_v : Atcflib.Vector.t;
      tmp0_v : Atcflib.Vector.t;
      tmp1_v : Atcflib.Vector.t;
      origin : float t_dims3;
      direction : float t_dims3;
      direction_recip : float t_dims3;
    }
    val recalculate : ?transform_i:Atcflib.Matrix.t -> t -> t
    val create : Atcflib.Vector.t -> Atcflib.Vector.t -> t
    val rect_intersect :
      ?transform_i:Atcflib.Matrix.t ->
      t -> float array -> float array -> float option
    val str : t -> string
  end
val str_av : Collider_ray.t * float -> string
