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
 * @file    stylesheet.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

(*a Includes *)
open Stylesheet
open Sax
open Ogl_types
open Utils
open Ogl_widget

(*a Builder module
 *
 * Permits an application to be built from a XML file
 *
 *)
module Builder = struct

  (*t stack_entry_element - element of the widget stack used in building the app from XML
   *)
  type stack_entry_element = ((string * string) list * (ogl_widget option))

  (*t stack_entry - list of widget/options that is a stack entry when building from XML
   *)
  type stack_entry         = stack_entry_element list

  (*t t - Application builder state
   *)
  type t = 
    {
      app_creator : (ogl_widget_display list) -> App.ogl_app;
      stylesheet : Stylesheet.Stylesheet.t;
      xml_additions : (string * (t -> string -> (string * string) list -> unit) ) list;
      mutable widget_stack : (string * Sax.attribute list) list;
      mutable children_stack : stack_entry list;
      mutable current_children : stack_entry;
      mutable displays : (ogl_widget_display list);
      mutable app : App.ogl_app option;
    }

  (*f create - create an application builder state
   *)
  let create app_creator stylesheet xml_additions= {app_creator; stylesheet; xml_additions; widget_stack=[]; children_stack=[]; current_children=[]; displays=[]; app=None}

  (*f list_head_tail - return list head and tail as a tuple
   *)
  let list_head_tail l = 
    match l with
      [] -> (Printf.printf "List empty\n%!"; raise Not_found)
    | hd::tail -> (hd, tail)

  (*f push_widget - push a widget+options on to the current stack_entry, and make it the current parent for children
   *)
  let push_widget t name atts=
    t.widget_stack <- (name, atts)::t.widget_stack;
    t.children_stack <- t.current_children::t.children_stack;
    t.current_children <- [];
    ()

  (*f add_child - add a child to the list of children of the current widget
   *)
  let add_child t widget =
    t.current_children <- ([], Some widget)::t.current_children;
    ()

  (*f add_grid_element - add options and a widget as a child of the current widget
   *)
  let add_grid_element t name_values child =
    t.current_children <- (name_values, Some child)::t.current_children;
    ()

  (*f add_grid_span - add options as a child of the current widget
   *)
  let add_grid_span t name_values =
    t.current_children <- (name_values, None)::t.current_children;
    ()

  (*f iter_children - apply a function to every widget in a child list
   *)
  let iter_children f children =
    List.iter (fun nvs_w ->
                let (_,opt_w)=nvs_w in
                if option_is_some opt_w then
                  let w = option_get opt_w in
                  f w
              )
              children

  (*f children_nth_widget - return the nth child widget option in  a children list
   *)
  let children_nth_widget n children =
    if ((List.length children)<=n) then
      None
    else
      let (_,opt_w) = List.nth children n in
      opt_w

  (*f pop_widget - get options and children list from top of children_stack and widget_stack
   *)
  let pop_widget t =
    let children = t.current_children in
    let (parents_children,rest_children) = list_head_tail t.children_stack in
    let ((name,atts),rest_widgets)       = list_head_tail t.widget_stack in
    t.children_stack <- rest_children;
    t.widget_stack <- rest_widgets;
    t.current_children <- parents_children;
    (name, atts, children)

  (*f make_app - Call the app creator on the displays already created, to create the app
   *)
  let make_app t children =
    t.app <- Some (t.app_creator t.displays);
    ()

  (*f add_display - Add a display to the end of the display list
   *)
  let add_display t display =
    display#set_depth 0;
    t.displays <- t.displays@[display];
    ()

  (*f get_opt_app - get the app option
   *)
  let get_opt_app t = 
    t.app

  (*f start_element - Invoke at XML open tag; push a widget on to the widget stack
   *)
  let start_element app ~uri ~localName ~qName ~atts =
    push_widget app localName atts;
    ()

  (*f end_element - Invoke at XML close tag; finalize the widget at the top of the widget stack
   *)
  let end_element app ~uri ~localName ~qName =
    let (st_name, atts, children) = pop_widget app in
    let name_values = Sax.Attributes.nameValues atts in
    if (List.mem_assoc st_name app.xml_additions) then (
      (List.assoc st_name app.xml_additions) app st_name name_values
    )
    else
      (
        match st_name with
          "label" -> 
          (
            let text     = (Sax.Attributes.getValue ~default:""   atts "text") in
            let widget = new ogl_widget_text app.stylesheet name_values in
            widget#name_value_args name_values;
            ignore (widget#set_text text);
            add_child app (widget :> ogl_widget)
          )
        | "window" -> 
           (
             let display = new ogl_widget_display app.stylesheet name_values None in
             display#name_value_args name_values;
             iter_children (fun w -> display#add_child w) children;
             add_display app display
           )
        | "box" -> 
           (
             let widget = new ogl_widget_box app.stylesheet name_values in
             widget#name_value_args name_values;
             iter_children (fun w -> widget#add_child w) children;
             add_child app widget
           )
        | "grid_element" -> 
           (
             let opt_w = children_nth_widget 0 children in
             if (option_is_some opt_w) then
               add_grid_element app name_values (option_get opt_w)
           )
        | "grid_span" -> 
           (
             let grid_span = (name_values) in
             add_grid_span app grid_span
           )
        | "grid" -> 
           (
             let widget = new ogl_widget_grid app.stylesheet name_values in
             widget#name_value_args name_values;
             let element_or_span_data nvs_ow =
               let (nvs,opt_w) = nvs_ow in
               if option_is_some opt_w then
                 let w = option_get opt_w in
                 (widget#set_element nvs w)
               else
                 (widget#set_span_data nvs)
             in
             List.iter element_or_span_data children;
             widget#grid_build;
             add_child app (widget :> ogl_widget)
           )
        | "app" -> 
           (
             make_app app children
           )
        | _ -> ()
      )

  (*f create_app_from_xml - Create an app from an XML *)
  let create_app_from_xml app_xml stylesheet xml_additions app_creator = 
    let app = create app_creator stylesheet xml_additions in
    let ogl_sax_content_handler =
      Content_handler.create
        ~start_element_callback:start_element
        ~end_element_callback:end_element
        ~callback_state:app
        ()
    in
    let ogl_sax_parser = Sax.XMLReader.create ~content_handler:ogl_sax_content_handler () in
    match Sax.XMLReader.parse_string ogl_sax_parser app_xml with
      Error msg -> ( Printf.printf "%s\n" msg; None )
    | Ok ()     -> ( get_opt_app app)

  (*f create_app_from_xml_file - Create an app from an XML file *)
  let create_app_from_xml_file app_xml_file stylesheet xml_additions app_creator = 
    let read_file f = 
      let rec read acc = 
      try
        let l = input_line f in
        read (l::acc)
      with _ -> acc
      in
      String.concat "\n" (List.rev (read []))
    in
    let app_xml = (read_file app_xml_file) in
    (*Printf.printf "%s\n" app_xml ;*)
    create_app_from_xml app_xml stylesheet xml_additions app_creator

    (*f All done *)
end

