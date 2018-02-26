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
 * @file          layout.ml
 * @brief         Modules to perform cubic table layout
 *
 *)

(*a Libraries *)
open Atcflib
open Tgl4
open Result
open Bigarray
open Font
open Utils
open Ogl_types

(*a Layout modules - layout t_ogl_widgets in a grid or by placement (NYI)
 *)
(*m Span - a span is part of a grid layout, for one dimension
    Each dimension has a span layout
    A span layout has a min and max elements, giving a base and length
    each element has a weight, a list of children that start there, and their spans, and their desired size.
    The element list should have one more entry appended (it will have no children)
    We work left to right through the element list e
      set e.start to be (previous.start) or 0 if first element
      Look at all the children c in element e;
        find the end element e' of c (e + span); update e' to have a minimum start of max(e.start+c.size; e'.start)
    Now the total desired size is the last element's start.
    Returns the dimensions
 *)
module Span = struct
  (*t Structure for a span element - used in array, one of these per element of the span*)
  type t_span_element = {
      mutable cl : (t_ogl_widget * int) list;
      mutable base_start : float;
      mutable base_size : float;
      mutable draw_start : float;
      mutable draw_size : float;
      mutable weight_grow : float;
      mutable weight_shrink : float;
    }

  (*t Structure for a span *)
  type t = {
      mutable element_list: (t_ogl_widget * float * int * int) list ;
      mutable num_elements : int ;
      mutable min_g : int ;
      mutable max_g : int ;
      mutable num_cells : int;
      mutable span_array: t_span_element array option ;
      mutable span_size : float;
      mutable default_weight_grow : float;
      mutable default_weight_shrink : float;
      mutable weights_grow : float list;
      mutable weights_shrink : float list;
      mutable total_weight_grow : float;
      mutable total_weight_shrink : float;
      mutable widget_sizes : (t_ogl_widget * float ref) list;
    }
  (*f create - create a span with no content *)
  let create _ = {
      element_list = [];
      num_elements = 0;
      min_g = 0 ;
      max_g = 0 ;
      num_cells = 0;
      span_array = None;
      span_size = 0.;
      default_weight_grow = 0.;
      default_weight_shrink = 0.;
      weights_grow = [];
      weights_shrink = [];
      total_weight_grow = 0.;
      total_weight_shrink = 0.;
    widget_sizes = [];
    }

  (*f set_weight *)
  let set_weight t grow shrink value_list =
    if ((List.length value_list)>0) then
    (
      if (grow)   then t.default_weight_grow   <- List.hd value_list;
      if (grow)   then t.weights_grow          <- value_list;
      if (shrink) then t.default_weight_shrink <- List.hd value_list;
      if (shrink) then t.weights_shrink        <- value_list;
    )
    else
    ()

  (*f set_data *)
  let nv_callbacks = [ ("weights",        fun t v -> (scan_float_list (set_weight t true true) v));
                       ("weights_grow",   fun t v -> (scan_float_list (set_weight t true false) v));
                       ("weights_shrink", fun t v -> (scan_float_list (set_weight t false true) v));
                     ]
  let set_data name_values t= 
    ignore (name_value_callbacks name_values nv_callbacks t);
    ()

  (*f add_place span widget size offset *)
  let add_place t w s o = ()

  (*f add_grid - add a widget (no known size as yet) with a base cell and a span size in cells *)
  let add_grid t w base span =
    if (t.num_elements=0) then
      begin
        t.min_g <- base;
        t.max_g <- base+span+1
      end
    else
      begin
        t.min_g <- min t.min_g base;
        t.max_g <- max t.max_g (base+span+1)
      end;
    t.num_elements <- t.num_elements + 1;
    t.element_list <- (w,0.,base,span)::(t.element_list)

  (*f build_span_array - build array of span elements, one per cell, and populate with widgets and weights; zero start/sizes
  *)
  let build_span_array t =
    t.num_cells <- t.max_g - t.min_g;
    if (t.num_cells=0) then
      (
        t.span_array <- None;
      )
    else
      (
        let mk_sae i = 
          let wg = if ((List.length t.weights_grow)<=i) then t.default_weight_grow else (List.nth t.weights_grow i) in
          let ws = if ((List.length t.weights_shrink)<=i) then t.default_weight_shrink else (List.nth t.weights_shrink i) in
          {cl=[]; weight_grow=wg; weight_shrink=ws; base_start=0.; base_size=0.; draw_start=0.; draw_size=0.} in
        let span_array = Array.init t.num_cells mk_sae in
        let fill_sae e =
          let (w,size,base,span) = e in
          let sae = span_array.(base - t.min_g) in
          sae.cl <- (w,span)::(sae.cl)
        in
        List.iter fill_sae t.element_list;
        t.span_array <- Some span_array;
      )

  (*f set_widget_size - set the desired size of a widget *)
  let set_widget_size t w size =
    if (not (List.mem_assoc w t.widget_sizes)) then
      t.widget_sizes <- (w,ref size)::t.widget_sizes
    else
      let rs = List.assoc w t.widget_sizes in
      rs := size

  (*f get_widget_size - set the desired size of a widget *)
  let get_widget_size t w  =
    if (not (List.mem_assoc w t.widget_sizes)) then 0.
    else
      let rs = List.assoc w t.widget_sizes in
      !rs

  (*f calculate_base_size - get the 'base_start' and 'base_size' array elements set up based on widget sizes *)
  let calculate_base_size t =
    if (option_is_none t.span_array) then
      (
        Printf.printf "base_size: Empty span array\n%!";
        0.
      )
    else
      let span_array = option_get (t.span_array) in
      let rec set_cell_start n earliest_start =
        if (n=t.num_cells) then earliest_start
        else begin
            let sae = span_array.(n) in
            sae.base_start <- max sae.base_start earliest_start;
            let set_endcell_starts w_sp =
              let (w,span) = w_sp in
              let size = get_widget_size t w in
              let esae = span_array.(n+span) in
              esae.base_start <- max esae.base_start (sae.base_start +. size)
            in
            List.iter set_endcell_starts sae.cl;
            set_cell_start (n+1) sae.base_start
          end
      in
      let rec set_cell_size_weights n tgw tsw = 
        if n=(t.num_cells-1) then (tgw, tsw)
        else begin
            let sae    = span_array.(n)   in
            let sae_p1 = span_array.(n+1) in
            let ntgw = tgw +. sae.weight_grow in
            let ntsw = tsw +. sae.weight_shrink in
            sae.base_size <- sae_p1.base_start -. sae.base_start;
            set_cell_size_weights (n+1) ntgw ntsw
          end
      in
      Array.iter (fun sae -> sae.base_start<-0.;) span_array;
      t.span_size <- set_cell_start 0 0.;
      let (tgw, tsw) = (set_cell_size_weights 0 0. 0.) in
      t.total_weight_grow <- tgw;
      t.total_weight_shrink <- tsw;
      (*Printf.printf "base_size of span:";*)
      Array.iteri (fun i sae -> (Printf.printf "%d:%f " i sae.base_start);()) span_array;
      (*Printf.printf "Size %f total_grow %f total_shrink %f\n%!" t.span_size tgw tsw;*)
      t.span_size

  (*f draw_size_span_array - get draw sizes and starts based on draw size dimension
    Start at -ddim/2 and progress across
    However, if the slack is not going to be picked up, then add that back in
    *)
  let draw_size_span_array t ddim =
    let slack = ddim -. t.span_size in
    let grow_per_unit =
     if ((t.total_weight_grow>0.) && (slack>0.)) then slack /. t.total_weight_grow else 0.
    in
    let shrink_per_unit =
     if ((t.total_weight_shrink>0.) && (slack<0.)) then slack /. t.total_weight_shrink else 0.
    in
    let slack_remaining = slack -. (shrink_per_unit *. t.total_weight_shrink) -. (grow_per_unit *. t.total_weight_grow)
    in
    if (option_is_none t.span_array) then
      ()
    else 
      let span_array = option_get (t.span_array) in
      let rec resize_cell n start =
        if (n=t.num_cells) then ()
        else begin
            let sae = span_array.(n) in
            sae.draw_start <- start;
            sae.draw_size <- sae.base_size +. (sae.weight_grow *. grow_per_unit) +. (sae.weight_shrink *. shrink_per_unit);
            resize_cell (n+1) (sae.draw_start +. sae.draw_size)
          end
      in
      resize_cell 0 ((ddim -. slack_remaining) *. (-. 0.5)) (* Start 'left' edge at -dim/2 *)

  (*f get_draw_size_offsets *)
  let get_draw_size_offsets t (f:'a -> float -> float -> unit) =
    if (option_is_none t.span_array) then
      ()
    else
      let span_array = option_get (t.span_array) in
      let draw_size_cells n sae =
        let min = sae.draw_start in
        let draw_size_cell cl = 
          let (w,span) =  cl in
          let max = span_array.(n+span).draw_start in
          let size = max -. min in
          let offset = (max +. min) /. 2. in
          f w size offset
        in
        List.iter draw_size_cell sae.cl
      in
      (*
      let display_cells n sae =
        Printf.printf "cell %d base %f/%f draw %f/%f\n%!" n sae.base_start sae.base_size sae.draw_start sae.draw_size
      in
      Array.iteri display_cells span_array;
      *)
      Array.iteri draw_size_cells span_array

  (*f All done *)
end

