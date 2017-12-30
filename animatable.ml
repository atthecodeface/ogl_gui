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
 * @file    animatable.ml
 * @brief   A library for animating things and sets of things
 *
 *)

(*a Helper functions and modules *)
(*f sfmt *)
let sfmt = Printf.sprintf

(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found


(*a Animatable module *)
module Animatable = struct  
  type set = {
    mutable elements : t list;
    mutable any_changing : bool
    }
  and t = {
    mutable target_time_ms : int;
    mutable last_time_ms : int;
    mutable changing : bool;
    mutable transition_fn : int -> int -> int -> bool (* last time, time now, target time *);
    mutable opt_parent : set option;
    }
  let create t_fn = {
    changing = false;
    transition_fn = (fun a b c -> true);
    target_time_ms = 0;
    last_time_ms = 0;
    opt_parent = None;
    }
  let set_recalculate t =
    t.any_changing <- List.fold_left (fun acc e -> (acc || e.changing)) false t.elements

  let set_transition transition_fn t =
    t.transition_fn <- transition_fn

  let time_step new_time_ms t =
    let finished = 
      if (new_time_ms >= t.target_time_ms) then
        (
          ignore (t.transition_fn t.last_time_ms t.target_time_ms t.target_time_ms);
          true
        )
      else
        (
          let is_done = t.transition_fn t.last_time_ms new_time_ms t.target_time_ms in
          t.last_time_ms <- new_time_ms;
          is_done
        )
    in
    if finished then
      (
        t.changing <- false;
        if (option_is_some t.opt_parent) then
          let parent = (option_get t.opt_parent) in
          set_recalculate parent
      )
    else
      ()

  let set_target time_now_ms target_time_ms t =
    t.last_time_ms <- time_now_ms;
    t.target_time_ms <- target_time_ms;
    if (option_is_some t.opt_parent) then
    (
      let parent = (option_get t.opt_parent) in
      set_recalculate parent
    );
    t.changing <- true
  let is_changing t = t.changing

  let set_create elements = 
    { elements; any_changing=false }
  let set_any_changing t = t.any_changing
  let add_element e t =
    t.elements <- e::t.elements;
    e.opt_parent <- Some t

  end
module Animatable_linear_float = struct
    type t = {
    anim : Animatable.t;
    current_value : float array;
    target_value : float array;
      }
    let set_value t v = Array.blit v 0 t.current_value 0 (Array.length t.current_value)
    let get_value t = t.current_value
    let transition_fn t last_ms now_ms target_ms =
      let past = float (now_ms - last_ms) in
      let total  = float (target_ms - last_ms) in
      let frac_t = past /. total in
      let frac_c = 1.0 -. frac_t in
      for i=0 to (Array.length t.current_value)-1 do
        t.current_value.(i) <- (frac_c *. t.current_value.(i)) +. (frac_t *. t.target_value.(i))
      done;
      (last_ms = target_ms)
    let create value = 
      let t ={
        anim = Animatable.create ();
        current_value = Array.copy value;
        target_value = Array.copy value;
      } in
      Animatable.set_transition (transition_fn t) t.anim ;
      t
    let is_changing t = Animatable.is_changing t.anim
    let time_step new_time_ms t = Animatable.time_step new_time_ms t.anim
    let set_target time_now_ms target_time_ms target_value t =
      Animatable.set_target time_now_ms target_time_ms t.anim;
      Array.iteri (fun n v -> t.target_value.(n)<-v) target_value
end

