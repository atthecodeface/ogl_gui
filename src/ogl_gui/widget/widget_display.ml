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
open Batteries
open Atcflib
open Tgl4
open Result
open Bigarray
open Font
open Utils
open Animatable
open Ogl_types
open Layout
open Obj
open Ogl_view
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a Styling *)
let widget_display_styles = [ ("width", Styleable_value.St_float);
                               ("height", Styleable_value.St_float);
    ] @ Widget_base.widget_base_styles
let styleable_widget_display_desc = Stylesheet.create_desc [Widget_base.styleable_act_level] widget_display_styles

(*a Classes *)
(*c ogl_widget_display - Display which contains a single widget
  This owns an OpenGL context and an openGL render buffer (which might
  be an OS window)

  A material is a program, a P, a G, and another 'other parameters' uniform
  P is always set to 'projection'
  G is a per-widget 'GUI' projection - this may rotate, translate, and so on, in to a [-1;+1]  cube
  Other parameters would include M and V for an 3D-world, a color for a material, and so on
  set_material <program> <G> <other> should
  check if program is current, and if not select it and set the P uniform
  It should set the G uniform and other uniform

  The 'draw_content' for a widget should set its material

  The decoration 'draw_border' and 'draw_background' should set their material
  *)
let screen_ppmm   = 5. (* 5 pixels per mm *)
let screen_2ppmm  = 2. *. screen_ppmm
let screen_depth_mm = 100. (* 10cm deep in, 10 cm out *)

class ogl_widget_display stylesheet name_values (toplevel_init:t_ogl_widget option): t_ogl_display = 
object (self)
  inherit Widget_base.ogl_widget stylesheet styleable_widget_display_desc "display" name_values  as super
  val mutable toplevel_widget : t_ogl_widget option = toplevel_init
  val mutable app : t_ogl_app option = None
  val projection = Matrix.make 4 4
  val screen_to_mm = Matrix.make 4 4
  val tmp_vector       = Vector.make 4
  val action_launch    = Vector.make 4
  val action_direction = Vector.make 4
  val play1 = Matrix.make 4 4
  val play2 = Matrix.make 4 4
  val playq1 = Quaternion.make_rijk 1.0 0.0 0.0 0.
  val playq2 = Quaternion.make_rijk 1.0 0.0 0.0 0.
  val mutable current_material : Glprogram.Material.t option = None
  initializer
    if option_is_some toplevel_init then
     super#add_child (option_get toplevel_init) ;
    ()

  method get_width_height =
    let flt_width  = Styleable_value.ref_value_as_float (Stylesheet.se_get_value_ref self#get_styleable "width") in
    let flt_height = Styleable_value.ref_value_as_float (Stylesheet.se_get_value_ref self#get_styleable "height") in
    (max (int_of_float flt_width) 80, max (int_of_float flt_height) 80)

  method create app_init =
    ignore (Quaternion.assign_of_rotation (Vector.make3 0.1 0.9 0.1) (cos 0.003) (sin 0.003) playq2) ;
    app <- Some app_init ;
    super#create app_init

  method display_reshape w h = (* Probably wants to become a set_bbox *)
    let w_mm = (float w) /. screen_ppmm in
    let h_mm = (float h) /. screen_ppmm in
    ignore Matrix.(identity projection |>
              set 0 0 (screen_2ppmm /. (float w)) |>
              set 1 1 (screen_2ppmm /. (float h)) |>
              set 2 2 (1. /. screen_depth_mm)) ;
    ignore Matrix.(identity screen_to_mm |>
              set 0 0 (1.0 /. screen_ppmm) |>
              set 0 3 (w_mm /. (-. 2.0)) |>
              set 1 1 (1.0 /. (-. screen_ppmm)) |>
              set 1 3 (h_mm /. 2.0) |>
              set 2 2 (1.)) ;
    let ddims = self#get_desired_dims in (* Propagates to children *)
    Printf.printf "Display desired dimensions %s\n" (str_fa ddims);
    self#layout [|w_mm; h_mm; screen_depth_mm|] Matrix.(identity (make 4 4)) [|0.0;0.0;0.|];
    Gl.viewport 0 0 w h ;
    ()

  method set_material opt_material widget_transformation =
    let transformation = ba_of_matrix4 widget_transformation in
    if ((Option.is_none current_material) || (Option.is_none opt_material) || ((Option.get opt_material) != (Option.get current_material))) then (
      current_material <- opt_material;
      Glprogram.Material.set_projection (option_get opt_material) (ba_of_matrix4 projection) transformation
    ) else (
      Glprogram.Material.set_transformation (option_get opt_material) transformation
    )
 
  method display_draw = 
    Gl.enable Gl.depth_test;
    Gl.clear_color 0.5 0.5 0.5 1.;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    ignore (Matrix.identity play1);
    ignore (Quaternion.premultiply playq2 playq1);
    ignore (Matrix.assign_from_q playq1 play1);
    ignore (Matrix.assign_m_m projection play1 play2); (* can set projection to be play2 *)
    current_material <- None;
    let view_set = Ogl_view.create (option_get app) (self :> t_ogl_display) (*display#set_material*) in
    self#draw view_set

  method private initial_action_vector x y =
    ignore (Vector.(set 0 (float x) tmp_vector |>
                      set 1 (float y) |>
                      set 2 (-. screen_depth_mm) |>
                      set 3 1.0));
    ignore (Vector.assign_m_v screen_to_mm tmp_vector action_launch);
    ignore (Vector.(set 0 0. action_direction |>
                      set 1 0. |>
                      set 2 (2. *. screen_depth_mm) |>
                      set 3 0.0));
    ((Collider_ray.create action_launch action_direction), 1.0)

  (*f display_key
    Propagate through children along the 'action_vector' - effectively where the key was pressed
    Of course a focus window could be set up to be the actual claimant of a key press

    The resulting callback of the propagation (key_result) can then be invoked

    The result of the callback could be a claim on future keypresses or somesuch
   *)
  method display_key     action k m x y =
    let action_vector = self#initial_action_vector x y in
    let key_result = self#key action k m action_vector in
    match key_result with
      Some da -> let (_,cb) = da in cb action k m action_vector
    |  _ -> None

  (*f display_mouse
    Propagate through children along the 'action_vector' - effectively where the mouse event occurred

    The resulting callback of the propagation (mouse_result) can then be invoked

    The result of the callback could be a claim on future mouse events
   *)
  method display_mouse action mouse x y options =
    let action_vector = self#initial_action_vector x y in
    let mouse_result  = self#mouse action mouse action_vector options in
    match mouse_result with
      Some da -> let (_,cb) = da in cb action mouse action_vector options
    |  _ -> McbNone

  (*f display_mouse_claimant
    The result of the callback could be a claim on future mouse events
   *)
  method display_mouse_claimant action mouse x y options callback =
    let action_vector = self#initial_action_vector x y in
    callback action mouse action_vector options

    method request_redraw = 
      match super#get_parent with
        Some widget -> widget#request_redraw
      | None -> (option_get app)#request_redraw (self:>t_ogl_display)
end

