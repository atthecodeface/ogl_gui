open Utils
open Animatable
open Stylesheet
open Atcflib
open Ogl_types
open Bigarray
class ogl_decoration :
  object
    val mutable angle : float
    val mutable bg_triangles : int
    val mutable border : float Utils.t_dims6
    val mutable border_color : Animatable_linear_float.t
    val mutable border_color_ref : Stylable_value_ref.t
    val mutable border_ref : Stylable_value_ref.t
    val mutable border_triangles : int
    val draw_transformation : Atcflib.Matrix.t
    val mutable face_color : float array
    val mutable face_color_ref : Stylable_value_ref.t
    val mutable gl_obj : Ogl_obj_standard.ogl_obj_geometry option
    val mutable margin : float Utils.t_dims6
    val mutable margin_ref : Stylable_value_ref.t
    val mutable padding : float Utils.t_dims6
    val mutable padding_ref : Stylable_value_ref.t
    val mutable time : int
    val tmp_vec_0 : Atcflib.Vector.t
    val tmp_vec_1 : Atcflib.Vector.t
    val mutable widget : Ogl_types.t_ogl_widget option
    method destroy : unit
    method draw_background :
      Ogl_types.t_ogl_app ->
      (float, Bigarray.float32_elt) Tgl4.Gl.bigarray -> unit
    method draw_border :
      Ogl_types.t_ogl_app ->
      (float, Bigarray.float32_elt) Tgl4.Gl.bigarray -> unit
    method private generate_vertices : float Utils.t_dims3 -> float array
    method get_content_offset : float array
    method get_decoration_dims : float array
    method register_widget : Ogl_types.t_ogl_widget -> unit Utils.ogl_result
    method set_layout :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float array -> unit
    method style_change :
      (Style_id.t * t_stylable_value) list -> unit
  end
