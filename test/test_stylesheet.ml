(** Copyright (C) 2017-2018,  Gavin J Stark.  All rights reserved.
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
 * @file          test_mesh.ml
 * @brief         Test suite for the mesh modules
 *
 *)

(*a Documentation
 * 
 * This is a test suite for the Ogl_gui stylesheet
 *
 *)

(*a Libraries used
 *
 * Alcotest is the test harness now
 *
 *)
open Ogl_gui

(*a Helper functions *)
(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found

(*f sfmt <string format> <arg> * -> string
 *    formats a string with arguments
 *)
let sfmt = Printf.sprintf

(*f stream_of_string *)
let stream_of_string test_str =
  let opt_char s n = if (n<(String.length s)) then (Some s.[n]) else None in
  Stream.from (opt_char test_str)

(*a Assertion functions, to simplify OUnit test code *)
(*f assert_equal_int : string -> int -> int -> unit
 *
 * Assert that the two ints are equal with a message msg and the two
 * float values
 * 
 *)
let assert_bool = Alcotest.(check bool)
let assert_equal_int msg v0 v1 =
  let close_enough = (v0 = v1) in
  assert_bool (sfmt "%s:%d:%d" msg v0 v1) true close_enough

(*a Simple test widget classes *)
(*c t_widget
 *)
class type t_widget = 
  object
    method get_styleable : Stylesheet.Styleable.t
    method set_parent : t_widget -> unit
    method get_parent : t_widget option
(*    method styleable_callback : Stylesheet.Styleable.t -> unit*)
  end

let wact_level = ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let wchecked   = ("checked",         [("checked",0); ("checked",1);])

(*v widget_*_styles *)
let widget_decorator_styles = [ ("padding", Styleable_value.St_float_6);
                         ("margin",  Styleable_value.St_float_6);
                         ("border",  Styleable_value.St_float_6);
                         ("faces",   Styleable_value.St_int_6);
                         ("border_color", Styleable_value.St_rgb );
                         ("face_color", Styleable_value.St_rgb );
             ]
(*c widget_base *)
let widget_base_styles = widget_decorator_styles @ [ ("dims", St_float_3); (* dims INCLUDING margin/border/padding - CSS box model *)
    (* relative size too? *)
                      ("offset", St_float_3 );
             ]
let widget_base_desc   = Stylesheet.Styleable_desc.create [] widget_base_styles
class widget_base stylesheet (name_values : (string * string) list) widget_type styleable_desc : t_widget  = 
object (self)
    val mutable parent : t_widget option = None
    val styleable = 
      Stylesheet.Styleable.create styleable_desc stylesheet widget_type name_values (fun e -> ()) []

(*    initializer
      Stylesheet.Styleable.set_callback styleable self#styleable_callback

    method styleable_callback (x:Styleable.t) = ()*)

    (*f get_styleable *)
    method get_styleable = styleable

    (*f set_parent - set the parent widget for this widget *)
    method set_parent widget = 
      Stylesheet.Styleable.set_parent widget#get_styleable styleable;
      parent <- Some widget

    (*f get_parent - get the parent widget that has been set *)
    method get_parent = parent

end

(*c widget_grid *)
let widget_grid_styles = widget_base_styles
let widget_grid_desc = Stylesheet.Styleable_desc.create [wact_level; wchecked] widget_grid_styles
class widget_grid stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "grid" widget_grid_desc as super
end

(*c widget_text *)
let widget_text_styles = widget_base_styles @ [ ("font_size", St_float);
                           ("font_thickness", St_float );
             ]
let widget_text_label_desc = Stylesheet.Styleable_desc.create [] widget_text_styles
class widget_text_label stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_label" widget_text_label_desc as super
end

(*c widget_text_button *)
let widget_text_button_desc = Stylesheet.Styleable_desc.create [wact_level] widget_text_styles
class widget_text_button stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_button" widget_text_button_desc as super
end

(*c widget_text_checkbutton *)
let widget_text_checkbutton_desc = Stylesheet.Styleable_desc.create [wact_level; wchecked] widget_text_styles
class widget_text_checkbutton stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_checkbutton" widget_text_checkbutton_desc as super
end

(*v stylesheet *)
let sel_cbox          =  Stylesheet.Styleable.is_element_id "control"
let sel_type_button   =  Stylesheet.Styleable.is_element_type "text_button"
let sel_cls_rotate    =  Stylesheet.Styleable.has_element_class "rotate"
let sel_state_hover   =  Stylesheet.Styleable.is_element_state 0 2
let sel_state_enable  =  Stylesheet.Styleable.is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let stylesheet = Stylesheet.Stylesheet.create ()
let _ =
    Stylesheet.Stylesheet.add_style_defaults stylesheet [
        ("border", Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
        ("padding", Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
        ("margin", Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
        ("dims",   Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
        ("offset", Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
        ("faces",  Styleable_value.Sv_int_6 [|0;0;0;0;0;0;|], false);
        ("face_color",   Styleable_value.Sv_rgb [|0.;0.;0.;|], false);
        ("border_color", Styleable_value.Sv_rgb [|0.;0.;0.;|], false);
        ("bg_color",     Styleable_value.Sv_rgb [|0.;0.;0.;|], false);
        ("font_size",    Styleable_value.Sv_float 0., false);
        ("font_thickness", Styleable_value.Sv_float 0., false);
      ];
    Stylesheet.Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Styleable_value.Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Styleable_value.Sv_float_6 [|2.;2.;2.;2.;2.;2.;|]);
             ];
    ()

(*a Stylesheet test suite *)
(*b Stylesheet test suite - combine individual suites *)
let base_state_descriptor = [
    ( "activity", [("disabled",0); ("enabled",1); ("hover",2); ("pressed",3)] );
    ( "checked", [("unchecked",0); ("checked",1)] );
    ( "focus", [("unfocused",0); ("focused",1)] );
                ]
let base_styles = [ ("border", Styleable_value.St_float_6);
    ("border_color", Styleable_value.St_rgb)
                  ]

(*a Top level *)
let act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let checked = ("checked", [("checked",0); ("checked",1);])
let box_state_desc = []
let label_state_desc = []
let button_state_desc = [act_level]
let chk_button_state_desc = [act_level; checked]

let styles = [ ("border", Styleable_value.St_float_6);
               ("border_color", Styleable_value.St_rgb );
             ]

let box_style_desc        = Stylesheet.Styleable_desc.create box_state_desc        styles
let label_style_desc      = Stylesheet.Styleable_desc.create label_state_desc      styles
let button_style_desc     = Stylesheet.Styleable_desc.create button_state_desc     styles
let chk_button_style_desc = Stylesheet.Styleable_desc.create chk_button_state_desc styles

let print_id e = Printf.printf "id %s\n" e.Stylesheet.Styleable.id_name

let wlbl_0 = new widget_text_label stylesheet [("id","lbl0");("border_color","0.5 0.2")]
let wlbl_1 = new widget_text_label stylesheet [("id","lbl1")]
let wlbt_2 = new widget_text_button stylesheet [("id","but2")]
let wbox_0 = new widget_grid       stylesheet [("id","control")]
let _ = 
  wlbl_0 # set_parent wbox_0 ;
  wlbl_1 # set_parent wbox_0 ;
  wlbt_2 # set_parent wbox_0 ;
    ()

let test_me _ =
  let root = [wbox_0#get_styleable] in
  let cbk_matching = Stylesheet.Stylesheet.element_callback_matching_tree stylesheet in
  ignore (Stylesheet.Stylesheet.build stylesheet root);
  Printf.printf "Printing all objects\n";
  ignore (cbk_matching (fun e -> true) print_id);
  Printf.printf "Printing all buttons\n";
  ignore (cbk_matching sel_type_button print_id);
  Printf.printf "Printing all items with id 'control' = should just be the one box\n";
  ignore (cbk_matching sel_cbox print_id);
  Printf.printf "Applying stylesheet\n";  
  Stylesheet.Stylesheet.apply_stylesheet stylesheet;
  Printf.printf "Applying stylesheet again\n";  
  Stylesheet.Stylesheet.apply_stylesheet stylesheet;
  Printf.printf "wlbl_0 border %s\n" (Styleable_value.str_of_svalue (Styleable_value.Styleable_value_ref.get_value (Stylesheet.Styleable.get_value_ref wlbl_0#get_styleable "border" )));
(*
  Printf.printf "wlbl_0 border color %s\n" (Styleable_value.str_of_svalue (Stylesheet.Styleable.get_value "border_color" wlbl_0#get_styleable));
  Printf.printf "wlbt_2 border %s\n" (Styleable_value.str_of_svalue (Stylesheet.Styleable.get_value "border" wlbt_2#get_styleable));
  Printf.printf "wlbt_2 border color %s\n" (Styleable_value.str_of_svalue (Stylesheet.Styleable.get_value "border_color" wlbt_2#get_styleable));
 *)
  Stylesheet.Styleable.set_element_state 0 2 (wlbt_2#get_styleable);
  Printf.printf "Applying stylesheet again\n";  
  Stylesheet.Stylesheet.apply_stylesheet stylesheet;
(*
  Printf.printf "wlbl_0 border %s\n" (str_of_svalue (Stylesheet.Styleable.get_value "border" wlbl_0#get_styleable));
  Printf.printf "wlbl_0 border color %s\n" (str_of_svalue (Stylesheet.Styleable.get_value "border_color" wlbl_0#get_styleable));
  Printf.printf "wlbt_2 border %s\n" (str_of_svalue (Stylesheet.Styleable.get_value "border" wlbt_2#get_styleable));
  Printf.printf "wlbt_2 border color %s\n" (str_of_svalue (Stylesheet.Styleable.get_value "border_color" wlbt_2#get_styleable));
 *)
  ()


(*a All test suites, toplevel *)
let test_suite = [
    "test_me", `Slow, test_me;
    ]

