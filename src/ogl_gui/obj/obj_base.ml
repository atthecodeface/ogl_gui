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
open Tgl4
open Result
open Bigarray
open Utils
open Ogl_types

(*a Types *)
type t_vap = int * int * Tgl4.Gl.enum * bool * int *int (* index size type_ normalized stride offset *)

(*a OpenGL object classes *)
(*c ogl_obj *)
class virtual ogl_obj  =
  object (self)

    (* Default object *)
    val mutable vao_glid = -1
    val mutable index_glid = -1
    val mutable vertex_data_glids = []
    val mutable opt_material : Glprogram.Material.t option = None

    (*f create_vao_2

      Create a vertex attribute object, using one or more float32 bigarrays.

      Each bigarray is used for a list of VAO pointers, specified individually
      by index, size (in TYPES), type, normalized (bool), stride (in BYTES) and offset (in BYTES)    
        
      *)
    method create_vao (vabs: ((t_vap list) * Utils.float32_bigarray) list) : unit Utils.ogl_result =

      (* Create and bind vao *)
      vao_glid <- gl_int_val (Gl.gen_vertex_arrays 1);
      Gl.bind_vertex_array vao_glid;

      (* Create vbos for each big_array element *)
      let num_attributes = List.length vabs in
      let vbo_glids      = ba_int32s num_attributes in
      Gl.gen_buffers num_attributes vbo_glids;

      (* Bind each VBO to its big_array element and attrib pointers *)
      let rec acc_bound_vbos buf_i vab acc =
        match vab with
        | [] -> acc
        | (vaps, ba)::vab_tl -> (
          let glid = Int32.to_int (vbo_glids.{buf_i}) in
          let size = Gl.bigarray_byte_size ba in
          Gl.bind_buffer Gl.array_buffer glid;
          Gl.buffer_data Gl.array_buffer size (Some ba) Gl.static_draw;
          let bind_vap (index,size,type_,normalized,stride,offset) =
            Gl.enable_vertex_attrib_array index;
            Gl.vertex_attrib_pointer index size type_ normalized stride (`Offset offset);
          in                                                                              
          List.iter bind_vap vaps;
          acc_bound_vbos (buf_i+1) vab_tl (glid::acc)
        )
      in 
      vertex_data_glids <- (acc_bound_vbos 0 vabs []);
      Ok () 

    (*f create_vao
    create a VAO with size (in TYPES), TYPE and bigarray for indices 0, 1, ...

    create_vao [(s0,t0,ba0), (s1,t1,ba1), ...]
    is equivalent to
    create_vao_2 [ ([(0,s0,t0,false,0,0)],ba0), ([(0,s1,t1,false,0,0)],ba1), ...]
    method create_vao (vertex_attribute_buffers: (int * Tgl4.Gl.enum * Utils.float32_bigarray) list) : unit Utils.ogl_result =
      vao_glid <- gl_int_val (Gl.gen_vertex_arrays 1);
        Gl.bind_vertex_array vao_glid;
        let num_attributes = List.length vertex_attribute_buffers in
        let vbo_glids = ba_int32s num_attributes in
        Gl.gen_buffers num_attributes vbo_glids;
        let rec do_first n vab acc =
          if (vab == []) then (acc) else (
            let (num,typ,ba)::vab_tl = vab in
            let glid = Int32.to_int (vbo_glids.{n}) in
            Gl.bind_buffer Gl.array_buffer glid;
            let size = Gl.bigarray_byte_size ba in
            Gl.buffer_data Gl.array_buffer size (Some ba) Gl.static_draw;
            Gl.enable_vertex_attrib_array n;
            Gl.vertex_attrib_pointer n num typ false 0 (`Offset 0);
            do_first (n+1) vab_tl (glid::acc)
          )
        in 
        vertex_data_glids <- (do_first 0 vertex_attribute_buffers []);
        Ok () 
     *)


      method add_indices_to_vao (indices:Utils.uint16_bigarray) =
        Gl.bind_vertex_array vao_glid;
        index_glid <- gl_int_val (Gl.gen_buffers 1);
        let size     = Gl.bigarray_byte_size indices in
        Gl.bind_buffer Gl.element_array_buffer index_glid;
        Gl.buffer_data Gl.element_array_buffer size (Some indices) Gl.static_draw;
        ()
      method add_indices32_to_vao (indices:Utils.int32_bigarray) =
        Gl.bind_vertex_array vao_glid;
        index_glid <- gl_int_val (Gl.gen_buffers 1);
        let size     = Gl.bigarray_byte_size indices in
        Gl.bind_buffer Gl.element_array_buffer index_glid;
        Gl.buffer_data Gl.element_array_buffer size (Some indices) Gl.static_draw;
        ()
    (*f bind_and_draw - bind vao, index vbo, and two float3 vbos and call draw callback*)
    method private bind_and_draw (draw_fn:unit->unit) : unit =
      let bind_attrib id loc dim typ =
        Gl.bind_buffer Gl.array_buffer id;
        Gl.enable_vertex_attrib_array loc;
        Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
      in

      Gl.bind_vertex_array vao_glid;
      Gl.bind_buffer Gl.element_array_buffer index_glid;
      List.iteri (fun i glid -> bind_attrib glid i 3 Gl.float) vertex_data_glids;

      draw_fn () ;
      Gl.bind_buffer Gl.array_buffer 0;
      Gl.bind_buffer Gl.element_array_buffer 0;
      Gl.bind_vertex_array 0;
      ()

    (*f create_geometry_from_indices - create vao, index vbo, two float3 vbos *)
    method private create_geometry_from_indices indices
                                                (vertex_data:float32_bigarray list)
                   : unit ogl_result  = 
      vao_glid    <- gl_int_val (Gl.gen_vertex_arrays 1);
      index_glid  <- gl_create_buffer indices ;
      vertex_data_glids <- [];
      List.iter (fun vfa -> vertex_data_glids <- vertex_data_glids @ [gl_create_buffer vfa]) vertex_data;
      Ok ()

    (*f delete_geometry - delete vao and vbos *)
    method delete_geometry : unit ogl_result = 
      if (vao_glid != -1) then (gl_with_int (Gl.delete_vertex_arrays 1) vao_glid);
      if (index_glid != -1) then (gl_delete_buffer index_glid);
      List.iter (fun glid -> gl_delete_buffer glid) vertex_data_glids;
      Ok ()

    (*f draw - must be supplied by concrete class *)
    method virtual draw : t_ogl_view_set -> int array -> unit

    (*f create_geometry - must be supplied by concrete class *)
    method virtual create_geometry : offset:float*float*float -> unit ogl_result

  (*f All done *)
  end

