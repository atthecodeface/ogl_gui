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
open Atcflib
open Tgl4
open Font
open Utils
open Animatable
open Ogl_types
open Obj
open Ogl_view
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a Styling *)
let font_default = option_get (Font.Outline.load_json "cabin-bold")
let widget_text_styles = [ ("font_color", Styleable_value.St_rgb);
                           ("font_size", Styleable_value.St_float);
                           ("font_height", Styleable_value.St_float);
                           ("font_thickness", Styleable_value.St_float);
    ] @ Widget_base.widget_base_styles
let styleable_widget_text_desc    = Stylesheet.create_desc [Widget_base.styleable_act_level] widget_text_styles

(*a Classes *)
(*c ogl_widget_text  *)
class ogl_widget_text stylesheet name_values =
  object (self)
    inherit Widget_base.ogl_widget stylesheet styleable_widget_text_desc "text" name_values   as super
    val mutable text = "not banana";
    val mutable font_size_ref       = Styleable_value.svr_zero;
    val mutable font_height_ref     = Styleable_value.svr_zero;
    val mutable font_thickness_ref  = Styleable_value.svr_zero;
    val mutable font_color_ref      = Styleable_value.svr_zero;
    val mutable font_size           = 0.;
    val mutable font_height         = 0.;
    val mutable font_thickness      = 0.; (* Currently not supported by ogl_obj_text *)
    val mutable font_color  = Animatable_linear_float.create [|0.;0.;0.;|]
    val mutable font = font_default;
    val mutable text_dims = [|0.;0.;0.|];
    val mutable obj = None;

  method create app =
    super#create app >>=
      fun _ ->
      (
        font_color_ref     <- Stylesheet.se_get_value_ref self#get_styleable "font_color" ;
        font_size_ref      <- Stylesheet.se_get_value_ref self#get_styleable "font_size" ;
        font_height_ref    <- Stylesheet.se_get_value_ref self#get_styleable "font_height" ;
        font_thickness_ref <- Stylesheet.se_get_value_ref self#get_styleable "font_thickness" ;
        font_size          <- Styleable_value.ref_value_as_float font_size_ref;
        font_height        <- Styleable_value.ref_value_as_float font_height_ref;
        font_thickness     <- Styleable_value.ref_value_as_float font_thickness_ref;
        Animatable_linear_float.set_value font_color (Styleable_value.ref_value_as_floats font_color_ref);
        self#set_text text
      )

  method set_text ?font_to_set text_to_set = 
    if (not self#can_create) then begin
        text <- text_to_set;
        Ok ()
    end
    else begin
        if (option_is_some obj) then (ignore ((option_get obj)#delete_geometry); obj <- None);
        if (option_is_some font_to_set) then font <- (option_get font_to_set);
        let fsize = font_size in
        let fheight = max font_size font_height in
        text <- text_to_set;
        let text_obj = new Obj.ogl_obj_text ~size:fsize ~height:fheight font text in
        let (x0,x1,y0,y1,z0,z1) = text_obj#get_bbox in
        text_dims <- [|x1 -. x0; y1 -. y0; z1 -. z0|];
        text_obj#create_geometry ~offset:(((x0 +. x1) *. (-. 0.5)), ((y0 +. y1) *. (-. 0.5)), 0.) >>=
          fun _ ->
          ( obj <- Some text_obj; Ok () )
    end
  method get_content_desired_dims = 
    text_dims
  method draw_content view_set transformation =
    if (option_is_none obj) then ()
    else
      (let other_uids = Ogl_view.set view_set (Ogl_view.get_material view_set "widget_color") transformation in
      let rgb = Animatable_linear_float.get_value font_color in
      Gl.uniform3f other_uids.(0) rgb.(0) rgb.(1) rgb.(2);
      (option_get obj)#draw view_set other_uids;
      Gl.bind_vertex_array 0;
      ())

    (*f style_change *)
    method style_change sid_svs =
      if (self#can_create) then (
        Animatable_linear_float.set_value font_color (Styleable_value.Styleable_value_ref.get_value_as_floats font_color_ref);
        super#style_change sid_svs;
      );
      ()

    (*f mouse - handle a mouse action along the action vector *)
    val mutable mouse_state = 0;
    method mouse_action action mouse vector options =
      let (cr,max_d) = vector in
      let opt_k = self#intersect_ray cr in
      match mouse_state with
        0 -> (if ((action<>Mouse_action_motion) || (option_is_none opt_k)) then McbNone else
                (mouse_state <- 1;Styleable.set_element_state 0 2 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
                 )
       | 1 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Styleable.set_element_state 0 1 self#get_styleable;Stylesheet.apply stylesheet;McbNone)
            else if (action=Mouse_action_down) then
             (mouse_state <- 2;Styleable.set_element_state 0 3 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
            else (McbSome (self#mouse_action))
           )
       | 2 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Styleable.set_element_state 0 1 self#get_styleable;Stylesheet.apply stylesheet;McbNone)
            else if (action=Mouse_action_up) then
              (
                (self#get_app)#button_pressed (self:>t_ogl_widget);
                mouse_state <- 1;Styleable.set_element_state 0 2 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
            else (McbSome (self#mouse_action))
           )
       | _ -> McbNone
    
    method mouse action mouse vector options =
      let (cr,max_d) = vector in
      match (self#intersect_ray cr) with
        None -> None
      | Some k -> Some (k, self#mouse_action)

    (*f All done *)
  end

