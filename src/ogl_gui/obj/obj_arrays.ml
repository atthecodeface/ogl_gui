(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
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
 * @file     obj_arrays.ml
 * @brief    Object arrays structure and functions to provide resizable index/coordinates for OpenGL objects
 *
 *)

type t_ba_float32s = (float, Bigarray.float32_elt,        Bigarray.c_layout) Bigarray.Array1.t
type t_ba_uint16s  = (int,   Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
let ba_float_array   len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)

module type Ogl_obj_array_config =
sig
  val fpc : int
end

module Ogl_obj_arrays (T:Ogl_obj_array_config) =
  struct
    type t = {
        mutable num_cs : int;
        mutable num_is : int;
        mutable max_cs : int;
        mutable max_is : int;
        expansion_factor : int; (* in tenths *)
        mutable triangle_strips : (int * int) list;
        mutable points          : (int * int) list;
        mutable cs   : t_ba_float32s;
        mutable is   : t_ba_uint16s;
      }

    let create num_cs num_is expansion_factor = 
      let cs = ba_float_array  (num_cs * T.fpc) in
      let is = ba_uint16_array (num_is        ) in
      { num_cs=0; num_is=0; expansion_factor; max_cs=num_cs; max_is=num_is; triangle_strips=[]; points=[]; cs; is; }

    let cs t = t.cs

    let is t = t.is

    let points t = t.points

    let strips t = t.triangle_strips

    let object_coord t = t.num_cs

    let object_index t = t.num_is

    let add_index t i =
      let n = t.num_is in
      t.num_is <- t.num_is + 1;
      t.is.{n} <- i;
      n

    let add_strip t i n = t.triangle_strips <- (i,n)::t.triangle_strips

    let add_points t i n = t.points <- (i,n)::t.points

    let expand_cs t num_cs =
      let max_cs = max num_cs ((t.max_cs * (t.expansion_factor + 10)) / 10) in
      let new_cs = ba_float_array  (max_cs * T.fpc) in
      let new_cs_start = Bigarray.Array1.sub new_cs 0 (t.max_cs*T.fpc) in
      Bigarray.Array1.blit t.cs new_cs_start;
      t.max_cs <- max_cs;
      t.cs <- new_cs;
      t

    let expand_is t num_is =
      let max_is = max num_is ((t.max_is * (t.expansion_factor + 10)) / 10) in
      let new_is = ba_uint16_array  (max_is) in
      let new_is_start = Bigarray.Array1.sub new_is 0 t.max_is in
      Bigarray.Array1.blit t.is new_is_start;
      t.max_is <- max_is;
      t.is <- new_is;
      t

    let ensure t num_cs num_is =
      let t = if (num_cs > t.max_cs) then (expand_cs t num_cs) else t in
      if (num_is > t.max_is) then (expand_is t num_is) else t

    let display t = 
      Printf.printf "Ogl_obj_arrays: %d/%d cs %d/%d is\n" t.num_cs t.max_cs t.num_is t.max_is;
      Printf.printf "  %d %d\n" (Bigarray.Array1.dim t.cs)  (Bigarray.Array1.dim t.is)
end

