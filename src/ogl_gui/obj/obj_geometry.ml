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
open Utils

(*a OpenGL object classes *)
(*c ogl_obj_geometry - N elements given vertices, colors and indices *)
class ogl_obj_geometry style num indices vertex_data =
  object (self)
    (*f subclass of ogl_obj *)
    inherit Obj_base.ogl_obj as super

    (*f create_geometry - build geometry from static object data with offset *)
    method create_geometry ~offset =
      self # create_vao vertex_data >>= fun _ -> 
      self # add_indices8_to_vao (ba_uint8s indices); Ok ()

    (*f draw - invoke super's draw with callback to draw elements once vao and vbos are bound *)
    method draw view_set other_uids =
      Gl.bind_vertex_array vao_glid;
      Gl.draw_elements style num Gl.unsigned_byte (`Offset 0)

    (*f draw_subset - draw a subset of the elements *)
    method draw_subset (view_set:Ogl_types.t_ogl_view_set) offset num =
      Gl.bind_vertex_array vao_glid;
      Gl.draw_elements style num Gl.unsigned_byte (`Offset offset)

    (*f All done *)
end

