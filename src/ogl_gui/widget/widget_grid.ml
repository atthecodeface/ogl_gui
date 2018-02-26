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
open Utils
open Animatable
open Ogl_types
open Layout
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a Classes *)
(*c ogl_widget_grid  *)
exception Bad_grid_layout of string
class ogl_widget_grid stylesheet name_values =
  
object (self)
  inherit Widget_base.ogl_widget stylesheet Styling.widget_grid "grid" name_values  as super

  val mutable grid_growth : (float t_dims3) option = None
  val mutable grid_shrink : (float t_dims3) option = None
  val mutable grid_base : (int t_dims3) option     = None
  val mutable grid_span : (int t_dims3)            = [|1;1;1;|]
  val mutable placement : (float t_dims3) option   = None
  val spans = [|Span.create (); Span.create (); Span.create (); |]


  (*f all_dims - invoke a function for all 3 dimensions *)
  method all_dims f =
    let rec do_next i =
      if (i=3) then ()
      else (f i; do_next (i+1))
    in
    do_next 0

  (*f set_span_data; name_values MUST have an 'axis':x|y|z *)
  method set_span_data name_values =
    let opt_axis = name_value_get name_values "axis" in
    if (option_is_none opt_axis) then
      raise (Bad_grid_layout "No 'axis' in span data");
    let axis = List.assoc "axis" name_values in
    let span = match axis with
        "x" -> spans.(0)
      | "y" -> spans.(1)
      | _ -> spans.(2)
    in
      Span.set_data name_values span

  (*f set_element *)
  method set_element name_values child =
    let base = [|0;0;0;|] in
    let span = [|1;1;1;|] in
    let nv_ele_callbacks = [ ("base", fun t v -> (scan_int3 (fun x y z -> (base.(0)<-x; base.(1)<-y; base.(2)<-z)) v));
                             ("span", fun t v -> (scan_int3 (fun x y z -> (span.(0)<-x; span.(1)<-y; span.(2)<-z)) v));
                           ]
    in
    ignore (name_value_callbacks name_values nv_ele_callbacks self);
    self#all_dims (fun i -> Span.add_grid spans.(i) child base.(i) span.(i));
    self#add_child child;
    ()

  (*f grid_build - Build the spans as required, without invoking children etc
     *)
  method grid_build =
    Array.iter Span.build_span_array spans;
    ()

  (*f grid_desired_size
    Returns the dimensions of the desired (packed) grid
     *)
  method grid_desired_size =
    let f acc c = 
      let cdims = (c#get_desired_dims) in
      self#all_dims (fun i -> Span.set_widget_size spans.(i) c cdims.(i))
    in
    List.fold_left f () self#get_children;
    let res = Array.make 3 0. in
    self#all_dims (fun i -> res.(i) <- Span.calculate_base_size spans.(i));
    res

  (*f layout_get_desired_dims - OVERRIDE of base widget - get desired dimensions give content and children *)
  method layout_get_desired_dims =
    let cdims         = self#get_content_desired_dims in (* in case there is some 'content' ... *)
    let children_dims = self#grid_desired_size in
    calc_dims1 cdims (fun a d -> max a d) children_dims

  (*f grid_resize
    Using the initial desired grid (as built), with new dimensions supplied, we can determine using the weights
    the slack, and the total weight, and the actual start points for each span element
    Center on 0,0,0.
   *)
  method grid_resize gdims =
    self#all_dims (fun i -> Span.draw_size_span_array spans.(i) gdims.(i); () )

  (*f grid_layout
    The resizes grid now has draw_start set for each span element.
    The total size of the grid might have grown to match the desired size
    Or it might not
    So now we need to call each individual content element with the actual position
    The draw_starts for the spans start at - (ddim - unconsumed slack)/2
   *)
  method grid_layout lmat =
    let widgets = ref [] in
    let f n w size offset = 
      if (n=0) then
        widgets := (w,((Array.make 3 0.),(Array.make 3 0.))) :: (!widgets) ;
      let (sizes,offsets) = (List.assoc w !widgets) in
      sizes.(n) <- size;
      offsets.(n) <- offset
    in
    self#all_dims (fun i -> Span.get_draw_size_offsets spans.(i) (f i); () );
    let set_widget_draw_size w_so =
      let (widget, (ldims, loffset)) = w_so in
      Printf.printf " >> %s : %s : %s\n%!" widget#str (str_fa ldims) (str_fa loffset);
      widget#layout ldims lmat loffset
    in
    List.iter set_widget_draw_size !widgets

  (*f layout_content_with_dims - OVERRIDE of base widget - set actual draw dimensions for content
   *)
  method layout_content_with_dims (ldims:float t_dims3) (lmat:Matrix.t) (loffset:float t_dims3) =
    let content_mat = self#set_layout ldims lmat loffset in
    Printf.printf "Grid resize and layout %s : %s\n" self#str (str_fa ldims);
    self#grid_resize ldims; (* Resize spans to fit based on weightings *)
    self#grid_layout content_mat;  (* Do final layout *)
    ()
  end

