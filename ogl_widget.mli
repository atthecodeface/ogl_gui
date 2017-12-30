(** Copyright (C) 2017,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file          ogl.ml
 * @brief         OpenGl framework library
 *
 *)

(*a Libraries *)
val font_default : Font.Outline.t
val create_stylesheet : 'a -> Stylesheet.Stylesheet.t
val widget_decorator_styles : (string * Stylesheet.t_stylable_type) list
val widget_base_styles : (string * Stylesheet.t_stylable_type) list
val stylable_act_level : string * (string * int) list
val widget_grid_styles : (string * Stylesheet.t_stylable_type) list
val widget_text_styles : (string * Stylesheet.t_stylable_type) list
val stylable_widget_box_desc : Stylesheet.Stylable_desc.t
val stylable_widget_grid_desc : Stylesheet.Stylable_desc.t
val stylable_widget_text_desc : Stylesheet.Stylable_desc.t
val stylable_widget_viewer_desc : Stylesheet.Stylable_desc.t
val stylable_widget_display_desc : Stylesheet.Stylable_desc.t
class ogl_widget :
  Stylesheet.Stylesheet.t ->
  Stylesheet.Stylable_desc.t ->
  string -> (string * string) list -> Ogl_types.t_ogl_widget
class ogl_widget_box :
  Stylesheet.Stylesheet.t ->
  (string * string) list ->
  object
    method add_child : ?order:int -> Ogl_types.t_ogl_widget -> unit
    method can_create : bool
    method create : Ogl_types.t_ogl_app -> unit Utils.ogl_result
    method create_tree_styles : unit Utils.ogl_result
    method destroy : unit
    method draw : Ogl_types.t_ogl_app -> Utils.float32_bigarray -> unit
    method draw_content :
      Ogl_types.t_ogl_app ->
      Utils.float32_bigarray -> Atcflib.Matrix.t -> unit
    method get_app : Ogl_types.t_ogl_app
    method get_children : Ogl_types.t_ogl_widget list
    method get_content_desired_dims : float Utils.t_dims3
    method get_content_draw_dims : float Utils.t_dims3
    method get_depth : int
    method get_desired_dims : float Utils.t_dims3
    method get_id : string
    method get_parent : Ogl_types.t_ogl_widget option
    method get_stylable : Stylesheet.Stylable.t
    method intersect_ray : Utils.Collider_ray.t -> float option
    method key :
      Ogl_types.t_key_action ->
      int -> int -> Ogl_types.t_action_vector -> Ogl_types.t_key_result
    method layout :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_content_with_dims :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_get_desired_dims : float Utils.t_dims3
    method mouse :
      Ogl_types.t_mouse_action ->
      int ->
      Ogl_types.t_action_vector -> int array -> Ogl_types.t_mouse_result
    method name_value_args : (string * string) list -> unit
    method request_redraw : unit
    method set_depth : int -> unit
    method set_layout :
      float Utils.t_dims3 ->
      Atcflib.Matrix.t -> float Utils.t_dims3 -> Atcflib.Matrix.t
    method set_parent : Ogl_types.t_ogl_widget -> unit
    method str : string
    method style_change : Stylesheet.Stylable.t_style_change_callback
  end
exception Bad_grid_layout of string
class ogl_widget_grid :
  Stylesheet.Stylesheet.t ->
  (string * string) list ->
  object
    val mutable grid_base : int Utils.t_dims3 option
    val mutable grid_growth : float Utils.t_dims3 option
    val mutable grid_shrink : float Utils.t_dims3 option
    val mutable grid_span : int Utils.t_dims3
    val mutable placement : float Utils.t_dims3 option
    val spans : Ogl_layout.Span.t array
    method add_child : ?order:int -> Ogl_types.t_ogl_widget -> unit
    method all_dims : (int -> unit) -> unit
    method can_create : bool
    method create : Ogl_types.t_ogl_app -> unit Utils.ogl_result
    method create_tree_styles : unit Utils.ogl_result
    method destroy : unit
    method draw : Ogl_types.t_ogl_app -> Utils.float32_bigarray -> unit
    method draw_content :
      Ogl_types.t_ogl_app ->
      Utils.float32_bigarray -> Atcflib.Matrix.t -> unit
    method get_app : Ogl_types.t_ogl_app
    method get_children : Ogl_types.t_ogl_widget list
    method get_content_desired_dims : float Utils.t_dims3
    method get_content_draw_dims : float Utils.t_dims3
    method get_depth : int
    method get_desired_dims : float Utils.t_dims3
    method get_id : string
    method get_parent : Ogl_types.t_ogl_widget option
    method get_stylable : Stylesheet.Stylable.t
    method grid_build : unit
    method grid_desired_size : float array
    method grid_layout : Atcflib.Matrix.t -> unit
    method grid_resize : float Utils.t_dims3 -> unit
    method intersect_ray : Utils.Collider_ray.t -> float option
    method key :
      Ogl_types.t_key_action ->
      int -> int -> Ogl_types.t_action_vector -> Ogl_types.t_key_result
    method layout :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_content_with_dims :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_get_desired_dims : float Utils.t_dims3
    method mouse :
      Ogl_types.t_mouse_action ->
      int ->
      Ogl_types.t_action_vector -> int array -> Ogl_types.t_mouse_result
    method name_value_args : (string * string) list -> unit
    method request_redraw : unit
    method set_depth : int -> unit
    method set_element :
      (string * string) list -> Ogl_types.t_ogl_widget -> unit
    method set_layout :
      float Utils.t_dims3 ->
      Atcflib.Matrix.t -> float Utils.t_dims3 -> Atcflib.Matrix.t
    method set_parent : Ogl_types.t_ogl_widget -> unit
    method set_span_data : (string * string) list -> unit
    method str : string
    method style_change : Stylesheet.Stylable.t_style_change_callback
  end
class ogl_widget_text :
  Stylesheet.Stylesheet.t ->
  (string * string) list ->
  object
    val mutable font : Font.Outline.t
    val mutable font_color : Animatable.Animatable_linear_float.t
    val mutable font_color_ref : Stylesheet.Stylable_value_ref.t
    val mutable font_height : float
    val mutable font_height_ref : Stylesheet.Stylable_value_ref.t
    val mutable font_size : float
    val mutable font_size_ref : Stylesheet.Stylable_value_ref.t
    val mutable font_thickness : float
    val mutable font_thickness_ref : Stylesheet.Stylable_value_ref.t
    val mutable mouse_state : int
    val mutable obj : Ogl_obj_standard.ogl_obj_text option
    val mutable text : string
    val mutable text_dims : float Utils.t_dims3
    method add_child : ?order:int -> Ogl_types.t_ogl_widget -> unit
    method can_create : bool
    method create : Ogl_types.t_ogl_app -> unit Utils.ogl_result
    method create_tree_styles : unit Utils.ogl_result
    method destroy : unit
    method draw : Ogl_types.t_ogl_app -> Utils.float32_bigarray -> unit
    method draw_content :
      Ogl_types.t_ogl_app ->
      Utils.float32_bigarray -> Atcflib.Matrix.t -> unit
    method get_app : Ogl_types.t_ogl_app
    method get_children : Ogl_types.t_ogl_widget list
    method get_content_desired_dims : float Utils.t_dims3
    method get_content_draw_dims : float Utils.t_dims3
    method get_depth : int
    method get_desired_dims : float Utils.t_dims3
    method get_id : string
    method get_parent : Ogl_types.t_ogl_widget option
    method get_stylable : Stylesheet.Stylable.t
    method intersect_ray : Utils.Collider_ray.t -> float option
    method key :
      Ogl_types.t_key_action ->
      int -> int -> Ogl_types.t_action_vector -> Ogl_types.t_key_result
    method layout :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_content_with_dims :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_get_desired_dims : float Utils.t_dims3
    method mouse :
      Ogl_types.t_mouse_action ->
      int ->
      Ogl_types.t_action_vector -> int array -> Ogl_types.t_mouse_result
    method mouse_action :
      Ogl_types.t_mouse_action ->
      int ->
      Ogl_types.t_action_vector ->
      int array -> Ogl_types.t_mouse_callback_result
    method name_value_args : (string * string) list -> unit
    method request_redraw : unit
    method set_depth : int -> unit
    method set_layout :
      float Utils.t_dims3 ->
      Atcflib.Matrix.t -> float Utils.t_dims3 -> Atcflib.Matrix.t
    method set_parent : Ogl_types.t_ogl_widget -> unit
    method set_text :
      ?font_to_set:Font.Outline.t -> string -> (unit, string) Result.result
    method str : string
    method style_change : Stylesheet.Stylable.t_style_change_callback
  end
val vector_x_axis : Atcflib.Vector.t
val vector_y_axis : Atcflib.Vector.t
val vector_z_axis : Atcflib.Vector.t
module Ordint : sig type t = int val compare : 'a -> 'a -> int end
module Intset :
  sig
    type elt = Ordint.t
    type t = Set.Make(Ordint).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
class ogl_widget_viewer :
  Stylesheet.Stylesheet.t ->
  (string * string) list ->
  object
    val centre_x : float ref
    val centre_y : float ref
    val centre_z : float ref
    val direction : Atcflib.Quaternion.t
    val mutable draw_fn : 'a -> 'b -> unit
    val mutable idler_handle : int
    val keys_down : Intset.t ref
    val mutable objs : Ogl_obj.ogl_obj list
    val mutable opt_program :
      (Ogl_program.Gl_program.t * int * int * int * int) option
    val q1 : Atcflib.Quaternion.t
    val q2 : Atcflib.Quaternion.t
    val q3 : Atcflib.Quaternion.t
    val rotation : Atcflib.Matrix.t
    val scale : float ref
    val tmp : Atcflib.Matrix.t
    val translation : Atcflib.Matrix.t
    val view : Atcflib.Matrix.t
    method add_child : ?order:int -> Ogl_types.t_ogl_widget -> unit
    method can_create : bool
    method create : Ogl_types.t_ogl_app -> unit Utils.ogl_result
    method create_geometry : unit
    method create_tree_styles : unit Utils.ogl_result
    method delete_geometry : unit
    method destroy : unit
    method draw : Ogl_types.t_ogl_app -> Utils.float32_bigarray -> unit
    method draw_content :
      Ogl_types.t_ogl_app ->
      Utils.float32_bigarray -> Atcflib.Matrix.t -> unit
    method get_app : Ogl_types.t_ogl_app
    method get_children : Ogl_types.t_ogl_widget list
    method get_content_desired_dims : float Utils.t_dims3
    method get_content_draw_dims : float Utils.t_dims3
    method get_depth : int
    method get_desired_dims : float Utils.t_dims3
    method get_id : string
    method get_parent : Ogl_types.t_ogl_widget option
    method get_stylable : Stylesheet.Stylable.t
    method private idle : unit -> int option
    method intersect_ray : Utils.Collider_ray.t -> float option
    method key :
      Ogl_types.t_key_action ->
      int -> int -> Ogl_types.t_action_vector -> Ogl_types.t_key_result
    method layout :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_content_with_dims :
      float Utils.t_dims3 -> Atcflib.Matrix.t -> float Utils.t_dims3 -> unit
    method layout_get_desired_dims : float Utils.t_dims3
    method mouse :
      Ogl_types.t_mouse_action ->
      int ->
      Ogl_types.t_action_vector -> int array -> Ogl_types.t_mouse_result
    method private move_forward : float -> unit
    method name_value_args : (string * string) list -> unit
    method private pitch : float -> unit
    method request_redraw : unit
    method private roll : float -> unit
    method set_depth : int -> unit
    method set_layout :
      float Utils.t_dims3 ->
      Atcflib.Matrix.t -> float Utils.t_dims3 -> Atcflib.Matrix.t
    method set_objs : Ogl_obj.ogl_obj list -> unit
    method set_parent : Ogl_types.t_ogl_widget -> unit
    method str : string
    method style_change : Stylesheet.Stylable.t_style_change_callback
    method private yaw : float -> unit
  end
val screen_ppmm : float
val screen_2ppmm : float
val screen_depth_mm : float
class ogl_widget_display :
  Stylesheet.Stylesheet.t ->
  (string * string) list ->
  Ogl_types.t_ogl_widget option -> Ogl_types.t_ogl_display
