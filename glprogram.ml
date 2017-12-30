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

(*a Libraries *)
open Tgl4

(*a Helper functions and modules *)
(*f sfmt *)
let sfmt = Printf.sprintf

(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*f ba_* - create Bigarrays from arrays of floats/ints/character length *)
let ba_floats fs  = Bigarray.(Array1.of_array float32 c_layout fs)
let ba_uint8s is  = Bigarray.(Array1.of_array int8_unsigned c_layout is)
let ba_string len = Bigarray.(Array1.create char c_layout len)
let ba_int32_1    = Bigarray.(Array1.create int32 c_layout 1)
let ba_float_array  len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array len = Bigarray.(Array1.create int16_unsigned c_layout len)

(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

(*f read_file - read a whole file in as a string *)
let read_file filename = 
  let f = open_in filename in
  let rec read acc =
    try read (acc ^ (input_line f) ^ "\n")
    with End_of_file -> close_in f ; acc
  in
  Ok (read "")

(*a Gl_program module *)
module Gl_program = struct
    type t = {
        prog_id     : int;
        attrib_ids  : (string * int) list;
        uniform_ids : (string * int) list;
      }
    type desc = {
        vertex_src : string;
        fragment_src : string;
        attribs : string list;
        uniforms : string list;
      }

    (*f make_desc *)
    let make_desc vertex_src fragment_src attribs uniforms = {vertex_src; fragment_src; attribs; uniforms}

    (*f compile_shader - Ocaml version of OpenGL.GL.shaders.compileShader
     Create shader, supply source, compile, then check result and return ogl_result *)
    let compile_shader src gl_type =
      let gl_id = Gl.create_shader gl_type in
      Gl.shader_source  gl_id src;
      Gl.compile_shader gl_id ;
      let compile_result res_type = gl_int_val (Gl.get_shaderiv gl_id res_type) in
      if (compile_result Gl.compile_status != Gl.true_) then
        begin
          let maxlen = compile_result Gl.info_log_length in
          let ba = ba_string maxlen in
          Gl.get_shader_info_log gl_id maxlen None ba;
          let log = Gl.string_of_bigarray ba in
          Gl.delete_shader gl_id;
          Error (sfmt "Compile error %s for %1000s\n" log src)
        end
      else
        Ok gl_id

    (*f compile_program - Ocaml version of OpenGL.GL.shaders.compileProgram
     Create program from shaders, link, then check result and return ogl_result *)
    let compile_program vert_id frag_id =
      let prog_id = Gl.create_program () in
      Gl.attach_shader prog_id vert_id;
      Gl.delete_shader vert_id;
      Gl.attach_shader prog_id frag_id;
      Gl.delete_shader frag_id;
      Gl.link_program  prog_id;
      let link_result res_type = gl_int_val (Gl.get_programiv prog_id res_type) in
      if link_result Gl.link_status != Gl.true_ then
        let maxlen = link_result Gl.info_log_length in
        let ba = ba_string maxlen in
        Gl.get_program_info_log prog_id maxlen None ba;
        let log = Gl.string_of_bigarray ba in
        Gl.delete_program prog_id;
        Error ("Link error" ^ log)
      else
        Ok prog_id

    (*f make - Ocaml version of opengl_app.c_opengl_shader.compile *)
    let make desc =
      let vertex_glsl_filename = desc.vertex_src in
      let fragment_glsl_filename = desc.fragment_src in
      let attribs = desc.attribs in
      let uniforms = desc.uniforms in
      read_file vertex_glsl_filename
      >>= fun vertex_src ->
      read_file fragment_glsl_filename
      >>= fun fragment_src ->
      compile_shader vertex_src Gl.vertex_shader
      >>= fun vert_id ->
      compile_shader fragment_src Gl.fragment_shader
      >>= fun frag_id ->
      compile_program vert_id frag_id
      >>= fun prog_id ->
      let attrib_ids = ref [] in
      let uniform_ids = ref [] in
      let append_attrib_id  attrib  = 
        let attrib_id = Gl.get_attrib_location prog_id attrib in
        attrib_ids  := (attrib,  attrib_id)::!attrib_ids  in
      let append_uniform_id uniform = 
        let uniform_id = Gl.get_uniform_location prog_id uniform in
        uniform_ids  := (uniform, uniform_id)::!uniform_ids  in
      List.iter append_attrib_id attribs ;
      List.iter append_uniform_id uniforms ;
      Ok { prog_id ;
           attrib_ids = !attrib_ids ;
           uniform_ids = !uniform_ids ;
         }

    (* delete - undo the make *)
    let delete t =
      Gl.delete_program t.prog_id;
      Ok ()

    (* get_attrib_id *)
    let get_attrib_id attrib t =
      List.assoc t.attrib_ids attrib

    (* get_uniform_id *)
    let get_uniform_id uniform t =
      List.assoc uniform t.uniform_ids 

end

