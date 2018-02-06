class virtual ogl_obj :
  object
    method virtual create_geometry : offset:float * float * float -> unit Utils.ogl_result
    method delete_geometry : unit Utils.ogl_result
    method create_vao : (int * Tgl4.Gl.enum * Utils.float32_bigarray) list -> unit Utils.ogl_result
    method add_indices_to_vao   : Utils.uint16_bigarray -> unit
    method add_indices32_to_vao : Utils.int32_bigarray -> unit
    method virtual draw : Ogl_types.t_ogl_view_set -> int array -> unit
    val mutable index_glid : int
    val mutable vao_glid : int
    val mutable vertex_data_glids : int list
    val mutable opt_material : Ogl_program.Material.t option
    method private create_geometry_from_indices :
      ('a, 'b) Tgl4.Gl.bigarray ->
      Utils.float32_bigarray list -> unit Utils.ogl_result
    method private bind_and_draw : (unit -> unit) -> unit
  end
