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
 * @file          texture.ml
 * @brief         Very simple module to create textures
 *
 *)

(*a Libraries *)
open Atcflib
open Tgl4
open Result
open Bigarray
open Utils
open Ogl_types

module Texture = struct
  let create_from_ba width height image_pixels =
    let gl_id = gl_int_val (Gl.gen_textures 1) in
    Gl.bind_texture Gl.texture_2d gl_id;
    Gl.tex_image2d  Gl.texture_2d 0 (*LOD*) Gl.rgb width height 0 (*border*) Gl.rgba (*data format*) Gl.unsigned_byte (`Data image_pixels);
    Printf.printf "texture %d\n" (Array1.size_in_bytes image_pixels);
    Printf.printf "texture %d %d %d\n" (image_pixels.{800}) (image_pixels.{801}) (image_pixels.{802});
    Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.mirrored_repeat;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.mirrored_repeat;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    gl_id

end
