class ogl_obj_geometry :
  Tgl4.Gl.enum ->
  int ->
  int array ->
  Utils.float32_bigarray list ->
  object
    val mutable index_glid : int
    val mutable vao_glid : int
    val mutable vertex_data_glids : int list
    method private bind_and_draw : (unit -> unit) -> unit
    method create_geometry :
      offset:float * float * float -> unit Utils.ogl_result
    method private create_geometry_from_indices :
      (int, Bigarray.int8_unsigned_elt) Tgl4.Gl.bigarray ->
      Utils.float32_bigarray list -> unit Utils.ogl_result
    method delete_geometry : unit Utils.ogl_result
    method draw : unit
    method draw_subset : int -> int -> unit
  end
