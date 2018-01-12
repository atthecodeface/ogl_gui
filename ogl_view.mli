module Ogl_view :
  sig
    val create : Ogl_types.t_ogl_app -> Ogl_types.t_ogl_display -> Ogl_types.t_ogl_view_set
    val get_material : Ogl_types.t_ogl_view_set -> string -> Ogl_program.Material.t option
    val set : Ogl_types.t_ogl_view_set -> Ogl_program.Material.t option -> Atcflib.Matrix.t -> int array
  end
