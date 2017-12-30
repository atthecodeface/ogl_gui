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
 * @file          test_mesh.ml
 * @brief         Test suite for the mesh modules
 *
 *)

(*a Documentation
 * 
 * This is a test suite for the ATCF XML library
 *
 * Features that require testing:
 *
 * Unicode
 *
 * reading of characters 0 to 127
 * reading of characters 0x80 to 0x7ff 
 * reading of characters 0x800 to 0xffff
 * reading of characters 0x10000 to 0x10ffff
 *)

(*a Libraries used
 *
 * OUnit is required as the test framework
 *
 *)
open Stylesheet
open OUnit

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
let assert_equal_int msg v0 v1 =
  let close_enough = (v0 = v1) in
  assert_bool (sfmt "%s:%d:%d" msg v0 v1) close_enough

(*a Simple test widget classes *)
(*c t_widget
 *)
class type t_widget = 
  object
    method get_stylable : Stylable.t
    method set_parent : t_widget -> unit
    method get_parent : t_widget option
    method stylable_callback : Stylable.t -> unit
  end

let wact_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let wchecked = ("checked", [("checked",0); ("checked",1);])

(*v widget_*_styles *)
let widget_decorator_styles = [ ("padding", St_float_6);
                         ("margin",  St_float_6);
                         ("border",  St_float_6);
                         ("faces",   St_int_6);
                         ("border_color", St_rgb );
                         ("face_color", St_rgb );
             ]
(*c widget_base *)
let widget_base_styles = widget_decorator_styles @ [ ("dims", St_float_3); (* dims INCLUDING margin/border/padding - CSS box model *)
    (* relative size too? *)
                      ("offset", St_float_3 );
             ]
let widget_base_desc   = Stylable_desc.create [] widget_base_styles
class widget_base stylesheet (name_values : (string * string) list) widget_type stylable_desc : t_widget  = 
object (self)
    val mutable parent : t_widget option = None
    val stylable = 
      Stylable.create stylable_desc stylesheet widget_type name_values (fun e -> ())

    initializer
      Stylable.set_callback stylable self#stylable_callback

    method stylable_callback (x:Stylable.t) = ()

    (*f get_stylable *)
    method get_stylable = stylable

    (*f set_parent - set the parent widget for this widget *)
    method set_parent widget = 
      Stylable.set_parent widget#get_stylable stylable;
      parent <- Some widget

    (*f get_parent - get the parent widget that has been set *)
    method get_parent = parent

end

(*c widget_grid *)
let widget_grid_styles = widget_base_styles
let widget_grid_desc = Stylable_desc.create [wact_level; wchecked] widget_grid_styles
class widget_grid stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "grid" widget_grid_desc as super
end

(*c widget_text *)
let widget_text_styles = widget_base_styles @ [ ("font_size", St_float);
                           ("font_thickness", St_float );
             ]
let widget_text_label_desc = Stylable_desc.create [] widget_text_styles
class widget_text_label stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_label" widget_text_label_desc as super
end

(*c widget_text_button *)
let widget_text_button_desc = Stylable_desc.create [wact_level] widget_text_styles
class widget_text_button stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_button" widget_text_button_desc as super
end

(*c widget_text_checkbutton *)
let widget_text_checkbutton_desc = Stylable_desc.create [wact_level; wchecked] widget_text_styles
class widget_text_checkbutton stylesheet (name_values : (string * string) list) = 
object (self)
  inherit widget_base stylesheet name_values "text_checkbutton" widget_text_checkbutton_desc as super
end

(*v stylesheet *)
let sel_cbox          =  Stylable.is_element_id "control"
let sel_type_button   =  Stylable.is_element_type "text_button"
let sel_cls_rotate    =  Stylable.has_element_class "rotate"
let sel_state_hover   =  Stylable.is_element_state 0 2
let sel_state_enable  =  Stylable.is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let stylesheet = Stylesheet.create ()
let _ =
    Stylesheet.add_style_defaults stylesheet [("border", Sv_float_6 [|0.;0.;0.;0.;0.;0.;|]);
                     ("padding", Sv_float_6 [|0.;0.;0.;0.;0.;0.;|]);
                     ("margin", Sv_float_6 [|0.;0.;0.;0.;0.;0.;|]);
                     ("dims",   Sv_float_3 [|0.;0.;0.;|]);
                     ("offset", Sv_float_3 [|0.;0.;0.;|]);
                     ("faces",  Sv_int_6 [|0;0;0;0;0;0;|]);
                     ("face_color",   Sv_rgb [|0.;0.;0.;|]);
                     ("border_color", Sv_rgb [|0.;0.;0.;|]);
                     ("bg_color",     Sv_rgb [|0.;0.;0.;|]);
                     ("font_size",    Sv_float 0.);
                     ("font_thickness", Sv_float 0.);
                    ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Sv_float_6 [|2.;2.;2.;2.;2.;2.;|]);
             ];
    ()

(*a Stylesheet test suite *)
(*b Stylesheet test suite - combine individual suites *)
let base_state_descriptor = [
    ( "activity", [("disabled",0); ("enabled",1); ("hover",2); ("pressed",3)] );
    ( "checked", [("unchecked",0); ("checked",1)] );
    ( "focus", [("unfocused",0); ("focused",1)] );
                ]
let base_styles = [ ("border", St_float_6);
    ("border_color", St_rgb)
                  ]

(*a Top level *)
let act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let checked = ("checked", [("checked",0); ("checked",1);])
let box_state_desc = []
let label_state_desc = []
let button_state_desc = [act_level]
let chk_button_state_desc = [act_level; checked]

let styles = [ ("border", St_float_6);
               ("border_color", St_rgb );
             ]

let box_style_desc        = Stylable_desc.create box_state_desc        styles
let label_style_desc      = Stylable_desc.create label_state_desc      styles
let button_style_desc     = Stylable_desc.create button_state_desc     styles
let chk_button_style_desc = Stylable_desc.create chk_button_state_desc styles

let print_id e = Printf.printf "id %s\n" e.Stylable.id_name

let wlbl_0 = new widget_text_label stylesheet [("id","lbl0");("border_color","0.5 0.2")]
let wlbl_1 = new widget_text_label stylesheet [("id","lbl1")]
let wlbt_2 = new widget_text_button stylesheet [("id","but2")]
let wbox_0 = new widget_grid       stylesheet [("id","control")]
let _ = 
  wlbl_0 # set_parent wbox_0 ;
  wlbl_1 # set_parent wbox_0 ;
  wlbt_2 # set_parent wbox_0 ;
    ()

let _ =
  let cbk_matching = Stylesheet.element_callback_matching_tree stylesheet in
  ignore (Stylesheet.build stylesheet);
  Printf.printf "Printing all objects\n";
  ignore (cbk_matching (fun e -> true) print_id);
  Printf.printf "Printing all buttons\n";
  ignore (cbk_matching sel_type_button print_id);
  Printf.printf "Printing all items with id 'control' = should just be the one box\n";
  ignore (cbk_matching sel_cbox print_id);
  Printf.printf "Applying stylesheet\n";  
  Stylesheet.apply_stylesheet stylesheet;
  Printf.printf "Applying stylesheet again\n";  
  Stylesheet.apply_stylesheet stylesheet;
  Printf.printf "wlbl_0 border %s\n" (str_of_svalue (Stylable.get_value "border" wlbl_0#get_stylable));
  Printf.printf "wlbl_0 border color %s\n" (str_of_svalue (Stylable.get_value "border_color" wlbl_0#get_stylable));
  Printf.printf "wlbt_2 border %s\n" (str_of_svalue (Stylable.get_value "border" wlbt_2#get_stylable));
  Printf.printf "wlbt_2 border color %s\n" (str_of_svalue (Stylable.get_value "border_color" wlbt_2#get_stylable));
  Stylable.set_element_state 0 2 (wlbt_2#get_stylable);
  Printf.printf "Applying stylesheet again\n";  
  Stylesheet.apply_stylesheet stylesheet;
  Printf.printf "wlbl_0 border %s\n" (str_of_svalue (Stylable.get_value "border" wlbl_0#get_stylable));
  Printf.printf "wlbl_0 border color %s\n" (str_of_svalue (Stylable.get_value "border_color" wlbl_0#get_stylable));
  Printf.printf "wlbt_2 border %s\n" (str_of_svalue (Stylable.get_value "border" wlbt_2#get_stylable));
  Printf.printf "wlbt_2 border color %s\n" (str_of_svalue (Stylable.get_value "border_color" wlbt_2#get_stylable));
  ()

let test_suite_stylesheet_parse =
  "parse" >::: [
(*  "blah" >:: fun _ ->
    let x = Stylable base_stylesheet base_state_descriptor base_styles "fred" ["wodget";] in
    x.get_style "border";
    x.set_state "activity" 1;
    ()
 *)
    ]

(*b Stylesheet test suite - combine individual suites *)
let test_suite_stylesheet =
  "Test stylesheet" >:::
    [
      test_suite_stylesheet_parse ;
    ]

(*a All test suites, toplevel *)
let test_suites = "All tests" >::: [
     test_suite_stylesheet ;
    ]


let _ = 
    at_exit Gc.full_major ;
    run_test_tt_main  test_suites ;
