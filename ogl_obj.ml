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

(*a OpenGL object classes *)
(*c ogl_obj *)
class virtual ogl_obj  =
  object (self)

    (* Default object *)
    val mutable vao_glid = -1
    val mutable index_glid = -1
    val mutable vertex_data_glids = []
    val mutable opt_program : Ogl_program.Gl_program.t option = None

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
      gl_with_int (Gl.delete_vertex_arrays 1) vao_glid;
      List.iter (fun glid -> gl_delete_buffer glid) vertex_data_glids;
      Ok ()

    (*f draw - must be supplied by concrete class *)
    method virtual draw : unit

    (*f create_geometry - must be supplied by concrete class *)
    method virtual create_geometry : offset:float*float*float -> unit ogl_result

  (*f All done *)
  end

