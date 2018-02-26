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

class ogl_widget         = Widget_base.ogl_widget
class ogl_widget_box     = Widget_base.ogl_widget_box
class ogl_widget_grid    = Widget_base.ogl_widget_grid
class ogl_widget_text    = Widget_base.ogl_widget_text
class ogl_widget_viewer  = Widget_base.ogl_widget_viewer
class ogl_widget_display = Widget_display.ogl_widget_display

let font_default = option_get (Font.Outline.load_json "cabin-bold")

(*a Stylesheet things *)
let create_stylesheet _ = 
  let stylesheet = Stylesheet.create () in
  Stylesheet.add_style_defaults stylesheet [("border",  Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("padding", Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("margin",  Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("dims",    Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("offset",  Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("align",   Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("faces",   Styleable_value.Sv_int_6 [|0;0;0;0;0;0;|], false);
                                            ("fill",    Styleable_value.Sv_int_3 [|0;0;0;|], false);
                                            ("width",   Styleable_value.Sv_float 0., false);
                                            ("height",   Styleable_value.Sv_float 0., false);
                                            ("face_color",   Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("border_color", Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("bg_color",     Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("font_size",    Styleable_value.Sv_float 1., true); (* inherit *)
                                            ("font_height",    Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_thickness", Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_color",    Styleable_value.Sv_rgb [|1.;1.;1.;|], true); (* inherit *)
                                           ];
    stylesheet

let widget_decorator_styles = [ ("padding", Styleable_value.St_float_6);
                         ("margin",  Styleable_value.St_float_6);
                         ("border",  Styleable_value.St_float_6);
                         ("faces",   Styleable_value.St_int_6);
                         ("border_color", Styleable_value.St_rgb );
                         ("face_color", Styleable_value.St_rgb );
             ]
let widget_base_styles = widget_decorator_styles @ [ ("dims", Styleable_value.St_float_3);
(* dims INCLUDING margin/border/padding - CSS box model *)
                      ("fill", Styleable_value.St_int_3 );
                      ("align", Styleable_value.St_float_3 );
                      ("offset", Styleable_value.St_float_3 );
             ]
let styleable_act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let widget_grid_styles = widget_base_styles
let widget_text_styles = [ ("font_color", Styleable_value.St_rgb);
                           ("font_size", Styleable_value.St_float);
                           ("font_height", Styleable_value.St_float);
                           ("font_thickness", Styleable_value.St_float);
    ] @ widget_base_styles
let styleable_widget_box_desc     = Stylesheet.create_desc [styleable_act_level] widget_base_styles
let styleable_widget_grid_desc    = Stylesheet.create_desc [styleable_act_level] widget_grid_styles
let styleable_widget_text_desc    = Stylesheet.create_desc [styleable_act_level] widget_text_styles
let styleable_widget_viewer_desc  = Stylesheet.create_desc [styleable_act_level] widget_base_styles

