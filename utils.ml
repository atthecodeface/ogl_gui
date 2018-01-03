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

type 'a ogl_result     = ('a, string) Result.result
type float32_bigarray  = (float, Bigarray.float32_elt,       Bigarray.c_layout) Bigarray.Array1.t

(*a Base types *)
(*t t_dims3/6 *)
type 'a t_dims3 = 'a array
type 'a t_dims6 = 'a array

(*a Helper functions and modules *)
(*f matrix_translate *)
let temp_matrix = Matrix.(identity (make 4 4))
let matrix_translate offset =
    Matrix.(identity temp_matrix |>
            set 0 3 offset.(0) |>
            set 1 3 offset.(1) |>
            set 2 3 offset.(2))

(*f sfmt *)
let sfmt = Printf.sprintf

(*f str_fa - string of float array *)
let str_fa fa = 
  let s = (Array.fold_left (fun s f -> sfmt "%s,%f" s f) "" fa)
  in String.sub s 1 ((String.length s)-1)

(*f str_ia - string of int array *)
let str_ia fa = 
  let s = (Array.fold_left (fun s f -> sfmt "%s,%d" s f) "" fa)
  in String.sub s 1 ((String.length s)-1)

(*f calc_dims_pair *)
let calc_dims_pair (f:float->float->float->float) dim acc =
    [| (f acc.(0) dim.(0) dim.(1));
       (f acc.(1) dim.(2) dim.(3));
       (f acc.(2) dim.(4) dim.(5)); |]

(*f calc_dims1 *)
let calc_dims1 acc f dim =
  [| (f acc.(0) dim.(0));
     (f acc.(1) dim.(1));
     (f acc.(2) dim.(2)); |]

(*f calc_dims2 *)
let calc_dims2 f a b =
  [| (f a.(0) b.(0));
     (f a.(1) b.(1));
     (f a.(2) b.(2)); |]

(*f calc_dims3 *)
let calc_dims3 f a b c =
  [| (f a.(0) b.(0) c.(0));
     (f a.(1) b.(1) c.(1));
     (f a.(2) b.(2) c.(2)); |]

(*f calc_dims4 *)
let calc_dims4 f a b c d =
  [| (f a.(0) b.(0) c.(0) d.(0));
     (f a.(1) b.(1) c.(1) d.(1));
     (f a.(2) b.(2) c.(2) d.(2)); |]

(*f nspaces *)
let rec nspaces n s =
  if (n<=0) then "" else
  nspaces (n-1) s ^ " "

(*f display_any_error *)
exception Display_error of string
let display_any_error res =
  (match res with Error s -> (Printf.printf "Display any error '%s'\n%!" s) | _ -> ();) ;
  res

(*f raise_any_error *)
let raise_any_error res =
  (match res with Error s -> (raise (Display_error s)) | _ -> ();) ;
  res

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
let ba_int32s len = Bigarray.(Array1.create int32 c_layout len)
let ba_float_array  len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array len = Bigarray.(Array1.create int16_unsigned c_layout len)

(*f gl_int_val, gl_with_int - to feed ints and get ints back from ctypes Opengl *)
let gl_int_val  f   = f ba_int32_1 ; Int32.to_int ba_int32_1.{0}
let gl_with_int f i = ba_int32_1.{0} <- Int32.of_int i; f ba_int32_1

(*f gl_create_buffer - create an OpenGL/Bigarray buffer of a certain byte size and bind them as static_draw *)
let gl_create_buffer byte_size =
  let bytes      = Gl.bigarray_byte_size byte_size in
  let gl_id      = gl_int_val (Gl.gen_buffers 1) in
  Gl.bind_buffer Gl.array_buffer gl_id;
  Gl.buffer_data Gl.array_buffer bytes (Some byte_size) Gl.static_draw;
  gl_id

(*f gl_delete_buffer - delete a buffer created by gl_create_buffer *)
let gl_delete_buffer gl_id =
  gl_with_int (Gl.delete_buffers 1) gl_id

(*f find_value *)
let find_value nvs name =
  let is_name acc nv =
    let (n,v) = nv in
    if (n=name) then (Some v) else acc
  in
  List.fold_left is_name None nvs

(*f trace *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f scan_value *)
let scan_value fmt f (v:string) =
  try
    let r = (Scanf.sscanf v fmt f) in
    (Some r)
  with
    Scanf.Scan_failure x ->
    (Printf.printf"Failed to parse value '%s'\n%!" x;
     None
    )

(*f skip_whitespace *)
let rec skip_whitespace string index =
  if (index >= String.length string) then
    index
  else if (string.[index]=' ') then (skip_whitespace string index+1)
  else index

(*f scan_float *)
let scan_float string index =
  let substring = String.sub string index ((String.length string)-index) in
  let f flt ofs = Some (flt, ofs+index) in
  try
    Scanf.sscanf substring " %f%n" f
  with
    Scanf.Scan_failure x -> None

(*f scan_float_list *)
let scan_float_list f v =
  let rec scan_next acc string index =
    let opt_value_index = scan_float string index in
    if option_is_none opt_value_index then
      acc
    else
      (
        let (value, index_after_value) = option_get opt_value_index in
        let next_index = skip_whitespace string index_after_value in
        let next_values = (acc @ [value]) in
        if (next_index >= String.length string) then
          next_values
        else if (string.[next_index]<>',') then
          next_values
        else
          (scan_next next_values string (next_index+1))
      )
    in
    let values = scan_next [] v 0 in
    Printf.printf "Got %d values\n" (List.length values);
    f values

(*f scan_float3 *)
let scan_float3 f v = (scan_value " %f , %f , %f" f v)

(*f scan_float6 *)
let scan_float6 f v = (scan_value " %f , %f , %f , %f , %f , %f" f v)

(*f scan_int3 *)
let scan_int3 f v = (scan_value " %d , %d , %d" f v)

(*f name_value_get *)
let name_value_get nvs name =
  if (List.mem_assoc name nvs) then
    Some (List.assoc name nvs)
  else
    None

(*f name_value_callbacks *)
let name_value_callbacks nvs nv_callbacks x =
  let find_callback acc nv =
    let (n,v) = nv in
    if (not (List.mem_assoc n nv_callbacks)) then
      acc
    else
      (
        let callback = (List.assoc n nv_callbacks) in
        callback x v;
        true
      )
  in
  List.fold_left find_callback false nvs

(*f identity4 *)
let identity4 = ba_floats[| 1.0; 0.0; 0.0; 0.0;
                            0.0; 1.0; 0.0; 0.0;
                            0.0; 0.0; 1.0; 0.0;
                            0.0; 0.0; 0.0; 1.0;|]
(*f ba_of_matrix4 *)
let ba_of_matrix4 m =     
      let get_v = Matrix.row_vector m in
      let v4 = [|Vector.coords (get_v 0);
                 Vector.coords (get_v 1);
                 Vector.coords (get_v 2);
                 Vector.coords (get_v 3);|] in
      ba_floats[| v4.(0).(0); v4.(0).(1); v4.(0).(2); v4.(0).(3); 
                           v4.(1).(0); v4.(1).(1); v4.(1).(2); v4.(1).(3); 
                           v4.(2).(0); v4.(2).(1); v4.(2).(2); v4.(2).(3); 
                           v4.(3).(0); v4.(3).(1); v4.(3).(2); v4.(3).(3); |]

(*m Collider_ray *)
module Collider_ray = struct
  type t = {
    origin_v    : Vector.t ;
    direction_v : Vector.t ;
    tmp0_v : Vector.t ;
    tmp1_v : Vector.t ;
    origin          : float t_dims3 ;
    direction       : float t_dims3 ;
    direction_recip : float t_dims3 ;
    }

  (*f recalculate *)
  let recalculate ?transform_i t = 
    let recip x = if (x=0.0) then (1.0E30) else (1.0 /. x) in
    if (option_is_none transform_i) then (
      Vector.(ignore (assign t.origin_v t.tmp0_v) ; ignore (assign t.direction_v t.tmp1_v))
    )
    else
      (
        let ti = option_get transform_i in
        Vector.(ignore (assign_m_v ti t.origin_v t.tmp0_v); ignore (assign_m_v ti t.direction_v t.tmp1_v))
      );
    for i=0 to 2 do 
        t.origin.(i)          <- Vector.get t.tmp0_v i;
        t.direction.(i)       <- Vector.get t.tmp1_v i;
        t.direction_recip.(i) <- recip t.direction.(i)
    done;
    t

  (*f create *)
  let create origin_v direction_v = 
    recalculate { origin_v; direction_v; tmp0_v=Vector.copy origin_v; tmp1_v=Vector.copy origin_v; origin = [|0.;0.;0.;|]; direction = [|0.;0.;0.|]; direction_recip = [|0.;0.;0.|]}

  (*f rect_instersect - find where along ray the box centered on (0,0) is struck*)
  let rect_intersect ?transform_i t offset dims = 
    ignore (recalculate ?transform_i t);
    (*Printf.printf "Intersect with %s @ %s : %s %s\n" (str_fa dims) (str_fa offset) (str_fa t.origin) (str_fa t.direction_recip);*)
    let minmax n = (* Find where along the ray it intersects with the planes on a pair of box edges *)
      let d0 = ((dims.(n) *. 0.5)      +. offset.(n) -. t.origin.(n)) *. t.direction_recip.(n) in
      let d1 = ((dims.(n) *. (-. 0.5)) +. offset.(n) -. t.origin.(n)) *. t.direction_recip.(n) in
      (min d0 d1, max d0 d1) in
    let (min_x, max_x) = minmax 0 in
    let (min_y, max_y) = minmax 1 in
    let (min_z, max_z) = minmax 2 in
    let maxmin = max (max min_x min_y) min_z in (* latest where it intersects all three dimensions at earliest *)
    let minmax = min (min max_x max_y) max_z in (* earliest where it intersects all three dimensions at latest *)
    if (minmax<0.) then (* if earliest it intersects the 'far side' of each dimension is behind the origin, then no intersection *)
      None
    else if (maxmin>minmax) then (* if latest it intersects ANY dimension's first face is after ANY dimension's last face then no intersection *)
      None
    else
    (
      (*Printf.printf "First intersection at %f (%f/%f %f/%f %f/%f)\n" maxmin min_x max_x min_y max_y min_z max_z;*)
      Some maxmin (* Okay, so hits all three dimensions once before it hits all three dimensions again; intersects first at the latest of the first intersection for every dimension *)
    )

   (*f str *)
  let str t = sfmt "%f,%f,%f : %f,%f,%f"
                   t.origin.(0) t.origin.(1) t.origin.(2)
                   t.direction.(0) t.direction.(1) t.direction.(2)
end

(*f str_av - string of action vector *)
let str_av av = 
  let (cr,m) = av in
  sfmt "%s : %f" (Collider_ray.str cr) m

