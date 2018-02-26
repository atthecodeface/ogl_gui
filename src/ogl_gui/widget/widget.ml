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
 * @file    widget.ml
 * @brief   Widgets available in the OpenGL GUI
 *
 *)

(*a Libraries *)
open Stylesheet
module Styleable = Stylesheet.Styleable

class ogl_widget         = Widget_base.ogl_widget
class ogl_widget_box     = Widget_box.ogl_widget_box
class ogl_widget_grid    = Widget_grid.ogl_widget_grid
class ogl_widget_text    = Widget_text.ogl_widget_text
class ogl_widget_viewer  = Widget_viewer.ogl_widget_viewer
class ogl_widget_display = Widget_display.ogl_widget_display

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
