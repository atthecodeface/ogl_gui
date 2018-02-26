open Batteries
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a Styling *)

let font_default = Option.get (Font.Outline.load_json "cabin-bold")

(*a Stylesheet things
 dimensions are  INCLUDING margin/border/padding - CSS box model 
 *)
let styleable_act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])


let widget_decorator_styles = [ ("padding", Styleable_value.St_float_6);
                         ("margin",  Styleable_value.St_float_6);
                         ("border",  Styleable_value.St_float_6);
                         ("faces",   Styleable_value.St_int_6);
                         ("border_color", Styleable_value.St_rgb );
                         ("face_color", Styleable_value.St_rgb );
             ]
let widget_base_styles = widget_decorator_styles @ [ ("dims", Styleable_value.St_float_3);
                      ("fill", Styleable_value.St_int_3 );
                      ("align", Styleable_value.St_float_3 );
                      ("offset", Styleable_value.St_float_3 );
             ]
let widget_display_styles = [ ("width", Styleable_value.St_float);
                               ("height", Styleable_value.St_float);
    ] @ widget_base_styles

let widget_text_styles = [ ("font_color", Styleable_value.St_rgb);
                           ("font_size", Styleable_value.St_float);
                           ("font_height", Styleable_value.St_float);
                           ("font_thickness", Styleable_value.St_float);
    ] @ widget_base_styles

let widget_grid_styles = widget_base_styles


let widget_text    = Stylesheet.create_desc [styleable_act_level] widget_text_styles
let widget_display = Stylesheet.create_desc [styleable_act_level] widget_display_styles
let widget_viewer  = Stylesheet.create_desc [styleable_act_level] widget_base_styles
let widget_grid    = Stylesheet.create_desc [styleable_act_level] widget_grid_styles
let widget_box     = Stylesheet.create_desc [styleable_act_level] widget_base_styles

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
