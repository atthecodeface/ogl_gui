class virtual ogl_obj :
  object
    method virtual create_geometry : offset:float * float * float -> unit Utils.ogl_result
    method delete_geometry : unit Utils.ogl_result
    method virtual draw : unit
    val mutable index_glid : int
    val mutable vao_glid : int
    val mutable vertex_data_glids : int list
    val mutable opt_program : Ogl_program.Gl_program.t option
    method private create_geometry_from_indices :
      ('a, 'b) Tgl4.Gl.bigarray ->
      Utils.float32_bigarray list -> unit Utils.ogl_result
    method private bind_and_draw : (unit -> unit) -> unit
  end
