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
open Font
open Glprogram
open Animatable
open Stylesheet

type 'a ogl_result     = ('a, string) Result.result
type float32_bigarray  = (float, Bigarray.float32_elt,       Bigarray.c_layout) Bigarray.Array1.t

(*a To do 
  Planes / faces not backgrounds
  Border  / frame / base color

  A border on a cuboid is a decoration of two the outer cuboid (frame minus margin)
    and the inner cuboid (frame minus margin minus border)

  Consider the left border decoration; it has sixteen coordinates:
    oo.i.j = outer outer = (fx.0,fy.i,fz.j)+(mx.0,my.i,mz.j)
    oi.i.j = outer inner = (fx.0,fy.i,fz.j)+(mx.0,my.i,mz.j)+(0,by.i,bz.j)
    io.i.j = inner outer = (fx.0,fy.i,fz.j)+(mx.0,my.i,mz.j)+(bx.0,0,0)
    ii.i.j = inner inner = (fx.0,fy.i,fz.j)+(mx.0,my.i,mz.j)+(bx.0,by.i,bz.j)
  Combining these appropriately yields 16 coordinates:

  One border decoration style can be cuboid. This would be 16 faces, 32 triangles
    forming the difference between a cuboid of the outer y/z between outer x and inner x,
    and the cuboid of the inner y/z between outer x and inner x.
    The 4 left-facing faces are:
      [oo.0.0; oi.0.0; oi.0.1; oo.0.1];
      [oo.0.1; oi.0.1; oi.1.1; oo.1.1]; 
      [oo.1.1; oi.1.1; oi.1.0; oo.1.0];
      [oo.1.0; oi.1.0; oi.0.0; oo.0.0]; 
    The 4 right-facing faces are:
      [io.0.0; ii.0.0; ii.0.1; io.0.1];
      [io.0.1; ii.0.1; ii.1.1; io.1.1]; 
      [io.1.1; ii.1.1; ii.1.0; io.1.0];
      [io.1.0; ii.1.0; ii.0.0; io.0.0]; 
    The 2 up-facing faces are:
      [oi.0.0; oi.0.1; ii.0.1; ii.0.0];
      [oo.1.0; oo.1.1; io.1.1; io.1.0];
    The 2 down-facing faces are:
      [oi.1.0; oi.1.1; ii.1.1; ii.1.0];
      [oo.0.0; oo.0.1; io.0.1; io.0.0];
    The 2 in-facing faces are:
      [oi.0.0; oi.1.0; ii.1.0; ii.0.0];
      [oo.0.1; oo.1.1; io.1.1; io.0.1];
    The 2 out-facing faces are:
      [oi.0.1; oi.1.1; ii.1.1; ii.0.1];
      [oo.0.0; oo.1.0; io.1.0; io.0.0];
    As a double-check, each vertex should be on 4 faces

  A simple decoration would be 4 planes:
    [oo.0.0; ii.0.0; ii.0.1; oo.0.1;];
    [oo.0.1; ii.0.1; ii.1.1; oo.1.1;];
    [oo.1.1; ii.1.1; ii.1.0; oo.1.0;];
    [oo.1.0; ii.1.0; ii.0.0; oo.0.0;];

  The border decoration is therefore created using 64 coordinates (4 * 4 * 4);
    for each dimension there is a outer.min; inner.min; inner.max; outer.max (in that numeric order too)

  These are then labeled C.I/O.0/1, with (C=x/y/z)
    C.o.0 = fC.0 + mC.0
    C.i.0 = fC.0 + mC.0 + bC.0
    C.i.1 = fC.1 - mC.1
    C.o.1 = fC.1 - mC.1 - bC.1

  The left border decoration then has:
    oo.i.j = x.O.0, y.O.i, z.O.j
    oi.i.j = x.O.0, y.I.i, z.I.j
    ii.i.j = x.I.0, y.I.i, z.I.j
    io.i.j = x.I.0, y.O.i, z.O.j

  The right border decoration has:
    oo.i.j = x.O.1, y.O.i, z.O.j
    oi.i.j = x.O.1, y.I.i, z.I.j
    ii.i.j = x.I.1, y.I.i, z.I.j
    io.i.j = x.I.1, y.O.i, z.O.j

  etc.

  Hence a decoration for a face is a function of the sixteen coordinates for that face, which are selected from the 64.
  Initially we can do this by creating an array of 64 vertices, numbering them (0..3, 0..3, 0..3) as (x,y,z) = x+4*y+16*z

  Then C.O.0 = 0, C.I.0=1, C.I.1=2, C.O.1=3 (so they are in numeric order)

  Hence the left border decoration has:
    oo.i.j = (0,0/3,0/3) = [0,12,48,60][j*2+i]
    oi.i.j = (0,1/2,1/2) = [4,8,20,28][j*2+i]
    io.i.j = (1,0/3,0/3) = [1,13,49,61][j*2+i]
    ii.i.j = (1,1/2,1/2) = [5,9,21,29][j*2+i]

  Now, the border decoration is going to create the faces; it can detect null faces by checking a face of 4 points (pt0, pt1, pt2, pt3)
    and if pt0 is the same as pt1, or pt0 is the same as pt3, then the face should not be added.

  Now a decoration of a side becomes a set of face indices appropriate to the decoration style

  More complex decorations are possible though; some by just applying a texture...


  *MISSING*
  Grid layout should have three weights x:y:z. for aspect ratio
  If an object has no desired aspect ratio then it uses 0:0:0
  If it desires a 1:1:1 aspect ratio then it uses 1:1:1
  Use that in growing/shrinking the cells somehow

  Thickness in text objects

  Mouse events return claim
  Claimed mouse events are called in preference to passing the ray (by the APP)
  Claimed mouse events can release (e.g. exiting object if in 'hover'; releasing mouse if in 'press')

  Font name
  Font weight
  Style classes
  Strings as styles
  Enumerations (set of strings) as styles
  None, Inherit as styles

  Style attributes for stylables that can be set in name/value that do not belong to the stylable so they can be inherited
 *)
(*a Base types *)
(*t t_dims3/6 *)
type 'a t_dims3 = 'a array
type 'a t_dims6 = 'a array

(*a Helper functions and modules *)
let font_default = Font.Outline.load_json "cabin-bold"

(*m Ordint, Intset - ordered integers, and a set of integers *)
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)

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

(*a Stylesheet things *)
let create_stylesheet _ = 
  let stylesheet = Stylesheet.create () in
  Stylesheet.add_style_defaults stylesheet [("border",  Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("padding", Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("margin",  Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("dims",    Sv_float_3 [|0.;0.;0.;|], false);
                                            ("offset",  Sv_float_3 [|0.;0.;0.;|], false);
                                            ("align",   Sv_float_3 [|0.;0.;0.;|], false);
                                            ("faces",   Sv_int_6 [|0;0;0;0;0;0;|], false);
                                            ("fill",    Sv_int_3 [|0;0;0;|], false);
                                            ("face_color",   Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("border_color", Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("bg_color",     Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("font_size",    Sv_float 1., true); (* inherit *)
                                            ("font_height",    Sv_float 0., true); (* inherit *)
                                            ("font_thickness", Sv_float 0., true); (* inherit *)
                                            ("font_color",    Sv_rgb [|1.;1.;1.;|], true); (* inherit *)
                                           ];
    stylesheet

let widget_decorator_styles = [ ("padding", St_float_6);
                         ("margin",  St_float_6);
                         ("border",  St_float_6);
                         ("faces",   St_int_6);
                         ("border_color", St_rgb );
                         ("face_color", St_rgb );
             ]
let widget_base_styles = widget_decorator_styles @ [ ("dims", St_float_3);
(* dims INCLUDING margin/border/padding - CSS box model *)
                      ("fill", St_int_3 );
                      ("align", St_float_3 );
                      ("offset", St_float_3 );
             ]
let stylable_act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let widget_grid_styles = widget_base_styles
let widget_text_styles = [ ("font_color", St_rgb);
                           ("font_size", St_float);
                           ("font_height", St_float);
                           ("font_thickness", St_float);
    ] @ widget_base_styles
let stylable_widget_box_desc     = Stylable_desc.create [stylable_act_level] widget_base_styles
let stylable_widget_grid_desc    = Stylable_desc.create [stylable_act_level] widget_grid_styles
let stylable_widget_text_desc    = Stylable_desc.create [stylable_act_level] widget_text_styles
let stylable_widget_viewer_desc  = Stylable_desc.create [stylable_act_level] widget_base_styles
let stylable_widget_display_desc = Stylable_desc.create [stylable_act_level] widget_base_styles

(*a OpenGL object classes *)
(*c ogl_obj *)
class virtual ogl_obj  =
  object (self)

    (* Default object *)
    val mutable vao_glid = -1
    val mutable index_glid = -1
    val mutable vertex_data_glids = []

    (*f draw_internal - bind vao, index vbo, and two float3 vbos and call draw callback*)
    method draw_internal (d:unit->unit) : unit =
      let bind_attrib id loc dim typ =
        Gl.bind_buffer Gl.array_buffer id;
        Gl.enable_vertex_attrib_array loc;
        Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
      in

      Gl.bind_vertex_array vao_glid;
      Gl.bind_buffer Gl.element_array_buffer index_glid;
      List.iteri (fun i glid -> bind_attrib glid i 3 Gl.float) vertex_data_glids;

      d () ;
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

    method draw_subset (offset:int) (num:int) = self#draw

  (*f All done *)
  end

(*c ogl_obj_geometry - N elements given vertices, colors and indices *)
class ogl_obj_geometry style num indices vertex_data =
  object (self)
    (*f subclass of ogl_obj *)
    inherit ogl_obj as super

    (*f create_geometry - build geometry from static object data with offset *)
    method create_geometry ~offset =
      super#create_geometry_from_indices (ba_uint8s indices)
                                         vertex_data

    (*f draw - invoke super's draw with callback to draw elements once vao and vbos are bound *)
    method draw =
      let d _ = 
        Gl.draw_elements style num Gl.unsigned_byte (`Offset 0);
        ()
      in self#draw_internal d

    (*f draw - invoke super's draw with callback to draw elements once vao and vbos are bound *)
    method draw_subset offset num =
      let d _ = 
        Gl.draw_elements style num Gl.unsigned_byte (`Offset offset);
        ()
      in self#draw_internal d

    (*f All done *)
end

(*c ogl_obj_text
 Only does zero thickness text at the moment
 *)
class ogl_obj_text ?size:(size=1.0) ?height:(height=1.0) (font:Font.Outline.t) (text:string) =
  object (self)

    (*f subclass of ogl_obj *)
    inherit ogl_obj as super

    val mutable draw_fn  = fun _ -> ();
  (*f build_text - accumulate (ofs, num_pts, pt_list, tri_list) for
  text, one character at at time *)
  val pts_tris =
    let build_text acc c =
      let (xofs, l, pt_list, tri_list) = acc in
      let renumber_tri t = let (a,b,c) = t in (a+l, b+l, c+l) in
      let (new_xofs, pts, tris) = Font.Outline.get_glyph_or_blank ~xofs:xofs ~xsc:size ~ysc:height font c in
      let new_tri_list = tri_list @ (List.map renumber_tri tris)  in
      (new_xofs, (l+(List.length pts)), pt_list @ pts, tri_list @ new_tri_list)
    in
    let rec string_fold ?n:(n=0) f acc text = 
      if n = String.length text then acc else
        let new_acc = f acc (String.make 1 text.[n]) in
        string_fold ~n:(n+1) f new_acc text
    in
    let _,_,pts,tris = string_fold build_text (0.,0,[],[]) text in
    (pts, tris) ;
      
  (*f get_bbox *)
  method get_bbox =
    let (pts, tris) = pts_tris in
    let do_pt acc pt =
      let (x,y) = pt in
      let (x0,x1,y0,y1,z0,z1) = acc in
      ((min x0 x), (max x1 x), (min y0 y), (max y1 y), (min z0 0.), (max z0 0.))
    in
    List.fold_left do_pt (0.,0.,0.,0.,0.,0.) pts
    
  (*f create_geometry - use 0 thickness to start with *)
  method create_geometry ~offset =
    let (pts, tris) = pts_tris in
    let num_pts = List.length pts in
    let num_tris = List.length tris in
    let vertices = ba_float_array (num_pts * 3) in
    let colors   = ba_float_array (num_pts * 3) in
    let indices  = ba_uint16_array (num_tris*3) in
    let (ox,oy,oz) = offset in
    let do_pt i pt =
      let (x,y) = pt in
      vertices.{i*3+0} <- x  +. ox;
      vertices.{i*3+1} <- y  +. oy;
      vertices.{i*3+2} <- 0. +. oz;
      colors.{i*3+0} <- 0. ;
      colors.{i*3+1} <- 1. ;
      colors.{i*3+2} <- 0.
    in
    let do_tri i tri =
      let (a,b,c) = tri in
      indices.{3*i+0} <- a ;
      indices.{3*i+1} <- b ;
      indices.{3*i+2} <- c ;
    in
    List.iteri do_pt pts ;
    List.iteri do_tri tris ;
    super#create_geometry_from_indices indices [vertices; colors] >>= fun _ ->
      let d _ = Gl.draw_elements Gl.triangles (num_tris*3) Gl.unsigned_short (`Offset 0);
                () in
      draw_fn <- d;
      Ok ()

  (*f draw - super's draw with a callback of the create_geometry resultant draw function *)
  method draw = self#draw_internal draw_fn

  (*f All done *)
end

(*a Class types for OpenGL widget, app and display, which are interrelated *)
(*t Enmerations *)
type t_mouse_action = 
  Mouse_action_down
| Mouse_action_up
| Mouse_action_wheel
| Mouse_action_motion

type t_key_action =
  Key_action_press
| Key_action_release

(*t t_window_handle - owned by ogl_app, given to OS-based infrastructure to permit it to control and report events to windows *)
type t_window_handle = int

(*t t_create_window_fn - a function of this type is provided by the OS-based infrastructure to allow the app to create a real OS window (and OpenGL context) *)
type t_create_window_fn = width:int -> height:int -> title:string -> t_window_handle -> t_window_handle ogl_result

(*c t_ogl_widget
  Units used by widgets are mm.

  The screen viewport is mapped from pixels to mm by the toplevel
  display.  Potentially a projection can scale the widgets, but that
  does not effect the widget itself.

  A widget has a desired size.

  This is derived from the larger of the widget's own content or its
  children laid out to their desired size.
  Added to this is the widget's decoration size (margin, border, padding)

  When a widget has been laid out it is given a view matrix (which is
  effectively how the widget is viewed), dimensions (in mm) for three
  dimensions, and an offset from (0,0,0) that the widget should be
  placed at. The widget has decoration, of course, so the content of
  the widget has potentially to be offset by a different amount if the
  decoration is lop-sided. The widget has children too; they are laid
  out within the dimensions, and they can then have their layout told
  to them with a new view matrix (that for this widget's content),
  dimensions (of the child widget as laid out), and offset (within the
  content of this widget).
  
 the placement layout at 2D is a, (x,y,w,h) for each widget, by name. The placement is attached to a 2D widget.
 x, y, w, h may be animatable or constant floats.
 the placement layout is informed of a placement of a widget using 'place <widget name> <x> <y> <w> <h>'
 if any of x,y,w,h change then the placer will ask its widget for a redraw, and the widget will ask its parent, and so on.

 the animatables must all be linked to an animator which is attached to the toplevel widget which can have an idler if any animatable is currently animating.

  a widget should be able to have the states Disabled, Highlighted, Normal

  as a result of the state the widget should have desired foreground color, background color, and border colors; complete with transparencies.
  The colors should therefore be animatable; if the state changes from one to another, then the colors change over a specified time.

  and animatable is then a value, a target value, and a time to get to that value, and an interpolation method.
  potentially text could have a rotation, which would be expressed as a quaternion, which could then be animatable

  a widget should be able to have a desired width / height / depth, or desired minimums for these. If not fixed then they should depend on the content (there own and children)

  a button widget should be able to have a checkbutton, radio button, radio button group, text, and additionaly children
  a checkbutton mark is a cuboid with a check object or cross object to the left, right, above, below, front or back of the content
  a radiobutton mark is a cuboid with a sphere object to the left, right, above, below, front or back of the content
  text may be aligned left, right, top, bottom, front, back
  text may have a single font, font size, thickness
  possibly an 'image' may be permitted which is a texture applied to a flat surface
 *)


type t_action_vector = (Collider_ray.t * float) (* launch point + direction collider, action range *)
type t_mouse_callback_result = McbNone | McbSome of (t_mouse_action -> int -> t_action_vector -> int array -> t_mouse_callback_result)
type t_mouse_claimant = t_mouse_action -> int -> t_action_vector -> int array -> (t_mouse_callback_result)
type t_mouse_callback = t_mouse_action -> int -> t_action_vector -> int array -> (t_mouse_callback_result)
type t_mouse_result = (float * t_mouse_callback) option
type t_key_callback = t_key_action -> int -> int -> t_action_vector -> int option
type t_key_result = (float * t_key_callback) option
class type t_ogl_widget = 
  object
    (* val children : ogl_widget list ;*)
    method name_value_args : (string * string) list -> unit
    method get_id : string
    method create_tree_styles : unit ogl_result (* Create tree of parent/children and stylables *)
    method create : t_ogl_app -> unit ogl_result (* After stylesheet, create objects, read styles etc - all set up *)
    method can_create : bool
    method get_app : t_ogl_app
    method destroy : unit
    method add_child : ?order:int -> t_ogl_widget -> unit
    method set_parent : t_ogl_widget -> unit
    method get_parent : t_ogl_widget option
    method get_children : t_ogl_widget list
    method get_depth : int
    method set_depth : int -> unit
    method get_stylable : Stylable.t
    method get_content_desired_dims : float t_dims3
    method get_content_draw_dims    : float t_dims3
    method get_desired_dims : float t_dims3
    method style_change : Stylable.t_style_change_callback
    method layout_get_desired_dims : float t_dims3  (* override with layout widget *)
    method layout           : float t_dims3 -> Matrix.t -> float t_dims3 -> unit
    method layout_content_with_dims  : float t_dims3 -> Matrix.t -> float t_dims3 -> unit (* override with layout widget *)
    method set_layout       : float t_dims3 -> Matrix.t -> float t_dims3 -> Matrix.t
    method draw             : t_ogl_app -> float32_bigarray -> unit (* app so that OpenGL may be interacted with *)
    method draw_content     : t_ogl_app -> float32_bigarray-> Matrix.t -> unit (* app so that OpenGL may be interacted with *)
    method intersect_ray    : Collider_ray.t -> float option
    method key              : t_key_action -> int -> int -> t_action_vector -> t_key_result
    method mouse            : t_mouse_action -> int -> t_action_vector -> int array -> t_mouse_result
    method request_redraw   : unit (* called by the widget or a child to request redraw *)
    method str : string
  end
(*c and t_ogl_display - a widget that is also an OS window and OpenGL context

  An ogl_display is an OS window, which is expected to contain one or more ogl_widgets

  The ogl_app despatches events to the requisite ogl_display based on a t_window_handle
  *)
and t_ogl_display = (* Widget that is a whole openGL context - or possibly fraction thereof *)
  object
    inherit t_ogl_widget
    method display_reshape : int -> int -> unit (* Reshapes the toplevel window *)
    method display_draw    : unit (* Draw the context; draw self and children with correct projection *)
    method display_key     : t_key_action -> int -> int -> int -> int -> int option
    method display_mouse   : t_mouse_action -> int -> int -> int -> int array -> t_mouse_callback_result
    method display_mouse_claimant : t_mouse_action -> int -> int -> int -> int array -> t_mouse_callback -> t_mouse_callback_result
  end

(*c and t_ogl_app - one of these is required for the application
  The app will be called by SDL or GLUT or whatever when a key is pressed, the mouse is moved, and so on, over any OS window
  *)
and t_ogl_app =
  object
    method set_create_window : t_create_window_fn -> unit (* Invoked by SDL/GLUT to populate the create_window function *)
    method create_window : ?width:int -> ?height:int -> ?title:string -> t_ogl_display -> t_window_handle ogl_result
    method create      : unit ogl_result
    method destroy     : unit ogl_result 
    method add_program : string -> Gl_program.desc -> unit ogl_result
    method add_display : string -> t_ogl_display   -> t_window_handle -> unit
    method get_program : string -> Gl_program.t
    method button_pressed : t_ogl_widget -> unit
    method key         : t_window_handle option -> t_key_action -> int -> int -> int -> int -> unit
    method mouse       : t_window_handle option -> t_mouse_action -> int -> int -> int -> int array -> unit
    method draw        : t_window_handle -> unit
    method reshape     : t_window_handle -> int -> int -> unit
    method request_redraw : t_ogl_display -> unit (* called by a toplevel widget to request redraw when need_redraw is set *)
    method need_redraw : t_window_handle list
    method idle        : int option
    method add_idler   : (unit -> int option) -> int
  end

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
      Printf.printf "base_size of span:";
      Array.iteri (fun i sae -> (Printf.printf "%d:%f " i sae.base_start);()) span_array;
      Printf.printf "Size %f total_grow %f total_shrink %f\n%!" t.span_size tgw tsw;
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
      let display_cells n sae =
        Printf.printf "cell %d base %f/%f draw %f/%f\n%!" n sae.base_start sae.base_size sae.draw_start sae.draw_size
      in
      Array.iteri display_cells span_array;
      Array.iteri draw_size_cells span_array

  (*f All done *)
end

(*a OpenGL widget decoration class - could use module for this, but be
consistent Separating it out from the widget allows for future
different decorators, and it simplifies the widget class itself.

     Need to add a decoration class with padding, border types and sizes, margins
     Needs a set_dims methods which sets the size of the widget's decorations and marks them as needing a rebuild
     Needs a draw_border app transform method which draws the border
     Needs a draw_background app transform method which draws the background

  *)
(*c ogl_decoration - borders and background planes for cuboids

  The border color can be None (nothing drawn) or an animatable color
  The background color can be None (nothing drawn) or an animatable color

 *)
class ogl_decoration =
object (self)
  (*t properties *)
  val mutable padding : (float t_dims6) = [|0.;0.;0.;0.;0.;0.|]
  val mutable margin  : (float t_dims6) = [|0.;0.;0.;0.;0.;0.|]
  val mutable border  : (float t_dims6) = [|0.;0.;0.;0.;0.;0.|]
  val mutable border_color  = Animatable_linear_float.create [|0.;0.;0.;|]
  val mutable face_color    = [|0.;0.;0.|]
  val mutable padding_ref      = svr_zero;
  val mutable margin_ref       = svr_zero;
  val mutable border_ref       = svr_zero;
  val mutable border_color_ref = svr_zero;
  val mutable face_color_ref   = svr_zero;
  val draw_transformation = Matrix.(identity (make 4 4))
  val mutable widget = None;
  val mutable gl_obj : ogl_obj_geometry option= None;
  val mutable bg_triangles = 0;
  val mutable border_triangles = 0;
  val tmp_vec_0 = Atcflib.Vector.make 3;
  val tmp_vec_1 = Atcflib.Vector.make 3;
         
  (*t register_widget - record the widget corresponding to this decoration 
    SHOULD BECOME CREATE *)
  method register_widget (w:t_ogl_widget) : unit ogl_result =
    widget <- Some w;
    padding_ref <- Stylable.get_value_ref w#get_stylable "padding" ;
    margin_ref  <- Stylable.get_value_ref w#get_stylable "margin" ;
    border_ref  <- Stylable.get_value_ref w#get_stylable "border" ;
    border_color_ref <- Stylable.get_value_ref w#get_stylable "border_color" ;
    face_color_ref   <- Stylable.get_value_ref w#get_stylable "face_color" ;
    border  <- Stylable_value_ref.get_value_as_floats border_ref;
    margin  <- Stylable_value_ref.get_value_as_floats margin_ref;
    padding <- Stylable_value_ref.get_value_as_floats padding_ref;
    Animatable_linear_float.set_value border_color (Stylable_value_ref.get_value_as_floats border_color_ref);
    face_color   <- Stylable_value_ref.get_value_as_floats face_color_ref;
    Ok ()

  (*f destroy *)
  method destroy = ()

  (*f style_change *)
  method style_change (sid_svs:(Style_id.t * t_stylable_value) list) =
    if (option_is_some widget) then (* created *)
    (
        Animatable_linear_float.set_value border_color (Stylable_value_ref.get_value_as_floats border_color_ref);
        face_color   <- Stylable_value_ref.get_value_as_floats face_color_ref;
    );
    ()

  (*f get_decoration dims - get desired dimensions of decoration *)
  method get_decoration_dims =
    let f_sum = (fun acc c0 c1 -> acc +.c0 +. c1) in
    [|0.;0.;0.|] |>
    (calc_dims_pair f_sum padding) |>
    (calc_dims_pair f_sum margin) |>
    (calc_dims_pair f_sum border)

  (*f get_content_offset - get offset from center of center-of-content *)
  method get_content_offset =
    let f_diff = (fun acc c0 c1 -> acc +.c0 -. c1) in
    [|0.;0.;0.|] |>
    (calc_dims_pair f_diff padding) |>
    (calc_dims_pair f_diff margin) |>
    (calc_dims_pair f_diff border)

  (*f generate_vertices - get border outer and inner (content) vertices
    This is a 64-float3 arrray, indexed by {z[2];y[2];x[2]}, i.e.
    float[0] is outer left front bottom, float[1] is inner left outer front bottom,
    float[2] is inner right outer front bottom, etc. - x is the bottom two most bits

    Outer is dims minus margin; inner is outer minus border size
 *)
  method private generate_vertices (dims:float t_dims3) =
    let xw = dims.(0) *. 0.5 in
    let yw = dims.(1) *. 0.5 in
    let zw = dims.(2) *. 0.5 in
    let xs = [|(-. xw) +. margin.(0); (-. xw) +. margin.(0) +. border.(0);
               xw -. margin.(1) -. border.(1); xw -. margin.(1) ;|] in
    let ys = [|(-. yw) +. margin.(2); (-. yw) +. margin.(2) +. border.(2);
               yw -. margin.(3) -. border.(3); yw -. margin.(3) ;|] in
    let zs = [|(-. zw) +. margin.(4); (-. zw) +. margin.(4) +. border.(4);
               zw -. margin.(5) -. border.(5); zw -. margin.(5) ;|] in
    let mk_vertex n =
      let v = n/3 in
      match (n mod 3) with
        0 -> xs.(v land 3)
      | 1 -> ys.((v lsr 2) land 3)
      | _ -> zs.((v lsr 4) land 3)
    in
    Array.init 192 mk_vertex

  (*f set_layout - set dimensions of real objects and transformation *)
  method set_layout (dims:float t_dims3) mat offset =
    ignore (Matrix.assign_m_m mat (matrix_translate offset) draw_transformation);
    if (option_is_some gl_obj) then
    begin
      let obj = option_get gl_obj in
      ignore (obj#delete_geometry);
      ()
    end;
    bg_triangles  <- 0;
    border_triangles <- 0;
    gl_obj <- None;
    if (border.(0)!=0.0) then
      begin
        let vertices = self#generate_vertices dims in
        let rect_index_array vertices index_array =
          let add_nonzero_rect acc rv =
            let (v0,v1,v2,v3) = rv in (* counter-clockwise, fwiw *)
            ignore Vector.(set 0 (vertices.(v1*3+0) -. vertices.(v0*3+0)) tmp_vec_0 |>
                              set 1 (vertices.(v1*3+1) -. vertices.(v0*3+1)) |>
                              set 2 (vertices.(v1*3+2) -. vertices.(v0*3+2)) );
            ignore Vector.(set 0 (vertices.(v3*3+0) -. vertices.(v0*3+0)) tmp_vec_1 |>
                              set 1 (vertices.(v3*3+1) -. vertices.(v0*3+1)) |>
                              set 2 (vertices.(v3*3+2) -. vertices.(v0*3+2)) );
            let dp = Atcflib.Vector.(modulus_squared (cross_product3 tmp_vec_0 tmp_vec_1)) in
            if (dp>1E-9) then 
              acc @ [v0;v1;v3;v1;v3;v2]
            else
              acc
          in
          Array.fold_left add_nonzero_rect [] index_array
        in
        let bg_indices = 
          if (face_color.(0)!=0.0) then
            (rect_index_array vertices [| (37,38,42,41) ;|]) (* z=2,y=1/2,x=1/2 *)
          else
            []
        in
        let border_indices = 
          if (border.(0)!=0.0) then
            (rect_index_array vertices [| (0,3,22,21); (3,15,26,22); (15,12,25,26); (12,0,21,25);
 (3,51,38,22); (51,63,42,38); (63,15,26,42);
 (48,0,21,37); (12,60,41,25); (60,48,37,41);
 (51,48,37,38); (60,63,42,41);
 |])
          else if (border.(0)!=0.0) then
            (rect_index_array vertices [| (0,3,6,5); (3,15,10,6); (15,12,9,10); (12,0,5,9) |]) (* front face only for now *)
          else
            []
        in
        bg_triangles  <- (List.length bg_indices) / 3;
        border_triangles <- (List.length border_indices) / 3;
        if ((bg_triangles + border_triangles)>0) then
          begin
            let obj = (new ogl_obj_geometry
                           Gl.triangles ((bg_triangles + border_triangles)*3) 
                           (Array.of_list (bg_indices @ border_indices))
                           [ba_floats vertices]
                      )
            in
            ignore (obj#create_geometry ~offset:(0.,0.,0.));
            gl_obj <- Some obj;
          end
      end;
    ()

  (*f draw_border - draw the border (of size in last 'set_dims' centred on 0,0) using transform *)
  val mutable angle = 0.;
  val mutable time = 0;
  method draw_border (app:t_ogl_app) projection =
    if ((option_is_none gl_obj) || (border_triangles=0)) then ()
    else
      (let program = app#get_program "widget_color" in
      let color_uid  = Gl_program.get_uniform_id "C"   program in
      let g_uid      = Gl_program.get_uniform_id "G"   program in
      let p_uid      = Gl_program.get_uniform_id "P"   program in
      Gl.use_program program.Gl_program.prog_id;
      if Animatable_linear_float.is_changing border_color then
         (Animatable_linear_float.time_step time border_color; time <- time + 1;);
(*    else
    (Animatable_linear_float.set_target 0 100 [|1.0-.bg_r;1.0-.bg_g;1.0-.bg_b|] border_color; time <- 0;)
);*)
      let bg_rgb = Animatable_linear_float.get_value border_color in
      let (bg_r, bg_g, bg_b) = (bg_rgb.(0), bg_rgb.(1), bg_rgb.(2)) in
      Gl.uniform3f color_uid bg_r bg_g bg_b;
      Gl.uniform_matrix4fv g_uid 1 true (ba_of_matrix4 draw_transformation);
      Gl.uniform_matrix4fv p_uid 1 true projection;
      angle <- angle +. 0.01;
      (option_get gl_obj)#draw_subset (bg_triangles*3) (border_triangles*3);
      Gl.bind_vertex_array 0;
      ())

  (*f draw_background - draw the background (of size in last 'set_dims' centred on 0,0) using transform *)
  method draw_background (app:t_ogl_app) projection =
    if ((option_is_none gl_obj) || (bg_triangles=0)) then ()
    else
      (let program = app#get_program "widget_color" in
      let color_uid  = Gl_program.get_uniform_id "C"   program in
      let g_uid      = Gl_program.get_uniform_id "G"   program in
      let p_uid      = Gl_program.get_uniform_id "P"   program in
      Gl.use_program program.Gl_program.prog_id;
      let bg_rgb = face_color in
      let (bg_r, bg_g, bg_b) = (bg_rgb.(0), bg_rgb.(1), bg_rgb.(2)) in
      Gl.uniform3f color_uid bg_r bg_g bg_b;
      Gl.uniform_matrix4fv g_uid 1 true (ba_of_matrix4 draw_transformation);
      Gl.uniform_matrix4fv p_uid 1 true projection;
      angle <- angle +. 0.01;
      (option_get gl_obj)#draw_subset 0 (bg_triangles*3);
      Gl.bind_vertex_array 0;
      ())

end

(*a OpenGL widget classes
  A widget represents a 3d box, with a possible thickness of 0 (which makes it 2D).
  It will have a certain size, which is specified to it by the set_bbox method invocation.

  The widget has decoration and content.

  The widget is drawn with an empty margin, border and padding, with internal content.

  Margin is not drawn - it is empty space

  Padding is not drawn - it is empty space

  Border is drawn if not none; the default style in 3D is 12 cuboids of
  the appropriate size; the top front cuboid is width
  (content+padding+border), height (border height), depth (border
  depth), for example.

  The content may be drawn with a content transformation (a 3x3 matrix, which should be invertible)

  The widget is drawn (using Z-buffering) decoration, content, then background.

  Background for zero thickness widgets is a rectangle. For non-zero
  thickness it is any set of the 6 sides of the widget box.

 *)
(*c ogl_widget, the base class  *)
class ogl_widget stylesheet stylable_desc widget_type name_values : t_ogl_widget = 
  
  object (self)
    val mutable parent : t_ogl_widget option = None
    val mutable children : t_ogl_widget list = []
    (* Style is based on state and activity 
     In essence a widget has a (id, class list, activity, state) list as a hierarchy to the widget
     The last of these is the leaf widget
     From this and a style sheet one should be able to deduce the style of the widget

     Can use app#get_style (widget, activity, state) call - but what does it return?
     We need (e.g.) decoration style (bg_color, border_color, padding, margin, border)

    The widget needs 'set state' and 'set activity' methods
    These would invoke methods for the decoration and content that permit 'style update'

    The outcome of a style update may be setting targets for animatables; so presumably style update needs a time in ms
    The widget needs an animatable set that the decoration and content can add to
    Then the widget needs a method to get 'animation in progress?'
    and a 'animate update (last_time, time_now)'.

     *)
    val mutable opt_stylable = None
    val mutable is_button = false (* true => permit hover and pressed activity levels through mouse interaction *)
    val mutable bbox_draw_dims    = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val mutable bbox_draw_offset  = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val temp_matrix = Matrix.(identity (make 4 4)) (* Applied to (0,0,0) places widget center in the correct place *)
    val bbox_transformation   = Matrix.(identity (make 4 4)) (* Applied to (0,0,0) places widget center in the correct place *)
    val bbox_transformation_i = Matrix.(identity (make 4 4)) (* Inverse of bbox_transformation, used to convert ray in parent coords to local coords *)
    val mutable content_draw_dims = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val content_transformation = Matrix.(identity (make 4 4)) (* Accounts for alignment of content if it does not grow *)
    val internal_transformation = Matrix.(make 4 4)
    val decoration   = new ogl_decoration;
    val mutable id = "uncreated";
    val mutable opt_app = None;

    val mutable depth = 0;

    val mutable dims_ref       = svr_zero;
    val mutable fill_ref       = svr_zero;
    val mutable align_ref      = svr_zero; (* -1.0 -> left, 0 center, 1.0 -> right *)

    val mutable dims     : (float t_dims3)          = [|0.;0.;0.|] 
    val mutable fill     : (int t_dims3)            = [|0;0;0;|]
    val mutable align    : (float t_dims3)          = [|0.;0.;0.|] 

    val mutable last_desired_dims = [|0.;0.;0.|] (* Desire, feed in to parent's layout *)

    (*f get_id *)
    method get_id = id

    (*f create_tree_styles *)
    method create_tree_styles =
      let create_if_ok acc child =
        acc >>= fun () -> child#create_tree_styles
      in
      List.fold_left create_if_ok (Ok ()) children >>= fun _ ->
      let stylable_children = List.map (fun w -> w#get_stylable) children in
      let stylable = Stylable.create stylable_desc stylesheet widget_type name_values (self#style_change) stylable_children in
      opt_stylable <- Some stylable;
      id <- sfmt "%s.%s" (Stylable.get_type stylable) (Stylable.get_id stylable);
      List.iter (fun w -> Stylable.set_parent stylable w#get_stylable) children;
      Ok ()

    (*f create - create children, and set up layout and decorations, returning error if required *)
    method create app_init =
      opt_app <- Some app_init;
      let create_if_ok acc child =
        acc >>= fun () -> child#create app_init
      in
      List.fold_left create_if_ok (Ok ()) children >>= fun _ ->
      let stylable = self#get_stylable in
      dims_ref  <- Stylable.get_value_ref stylable "dims" ;
      fill_ref  <- Stylable.get_value_ref stylable "fill" ;
      align_ref <- Stylable.get_value_ref stylable "align" ;
      dims <- Stylable_value_ref.get_value_as_floats dims_ref;
      align <- Stylable_value_ref.get_value_as_floats align_ref;
      fill <- Stylable_value_ref.get_value_as_ints fill_ref;
      decoration#register_widget (self :> t_ogl_widget)

    (*f style_change *)
    method style_change sid_svs =
      if (option_is_some opt_app) then decoration#style_change sid_svs;
      ()

    (*f name_value_args - parse any name/value argument string pairs *)
    method name_value_args name_values = 
      ()

    (*f can_create - return True if the widget has been created; at this point OpenGL callls can be invoked *)
    method can_create = (option_is_some opt_app)

    (*f get_app - return the app *)
    method get_app = (option_get opt_app)

    (*f destroy - destroy the widget, its decoration, and all its children *)
    method destroy =
      decoration#destroy ;
      List.iter (fun c -> c#destroy) children

    (*f add_child - add a child widget to the widget (in the future at a certain order point) *)
    method add_child ?order:(order=0) widget = 
      children <- widget :: children ;
      widget#set_parent (self:>t_ogl_widget) (* self can be a subclass of t_ogl_widget...*)

    (*f set_depth - set the depth of the widget in the tree and its children *)
    method set_depth d =
      depth <- d;
      List.iter (fun c -> c#set_depth (depth + 1)) children

    (*f get_depth - get the depth of the widget in the tree *)
    method get_depth = depth

    (*f get_stylable - get the Stylable that the widget is *)
    method get_stylable = (option_get opt_stylable)

    (*f set_parent - set the parent widget for this widget *)
    method set_parent widget = 
      parent <- Some widget

    (*f get_parent - get the parent widget that has been set *)
    method get_parent = parent

    (*f get_children - get the children of the widget *)
    method get_children = children

    (*f get_desired_dims - get the desired dimensions of the widget - the size of its children laid out or its content plus decoration *)
    method get_desired_dims = 
      let cdims = self#layout_get_desired_dims in
      let ddims = decoration#get_decoration_dims in
      let actual_dims = 
        if (dims.(0)>1.0) then
           dims
        else
          calc_dims1 ddims (fun a d->a+.d) cdims
      in
      last_desired_dims <- actual_dims;
      actual_dims

    (*f layout_get_desired_dims - OVERRIDE with layout widget - get desired dimensions give content and children *)
    method layout_get_desired_dims =
      let children_dims =
        List.rev_map (fun w -> w#get_desired_dims) children
      in
      List.fold_left (fun acc cdims->calc_dims1 acc (fun a d -> max a d) cdims) (self#get_content_desired_dims) children_dims

    (*f apply_fill
     For each dimension:
       If need to shrink and (fill&2) then shrink
       Else if need to grow and (fill&1) then grow
    *)
    method private apply_fill dims ddims = 
      let fill_dim draw desire fill = 
        if (((fill land 2)<>0) && (draw<desire)) then draw
        else if (((fill land 1)<>0) && (draw>desire)) then draw
        else desire
      in
      calc_dims3 fill_dim dims ddims fill
    
      (*f apply_align
      For each dimension:
        Find the slack;
        If align=-1 then subtract half the slack; if align=1 then add half the slack;
        So add slack/2 * align
    *)
    method private apply_align offset dims ddims =
      calc_dims4 (fun offset draw desire align -> ((draw-.desire) *. 0.5) *. align +. offset) offset dims ddims align
    
    (*f layout - lay the widget content out given actual draw dimensions, offset to center of that layout, and a base matrix *)
    method layout dims mat offset =
      Printf.printf "%sLayout %s : %s : %s\n%!" (nspaces (depth*3) "") self#str (str_fa dims) (str_fa offset);
      bbox_draw_dims   <- self#apply_fill   dims last_desired_dims;
      bbox_draw_offset <- self#apply_align  offset dims bbox_draw_dims;
      Matrix.(ignore (assign mat bbox_transformation);
              ignore (assign mat bbox_transformation_i |> lup_invert));
      let dec_dims       = decoration#get_decoration_dims in
      let content_dims   = calc_dims1 bbox_draw_dims (fun a d -> (a -. d)) dec_dims in
      let dec_offset     = decoration#get_content_offset in
      let content_offset = calc_dims1 bbox_draw_offset (fun a d -> (a +. d)) dec_offset in
      decoration#set_layout bbox_draw_dims mat bbox_draw_offset;
      self#layout_content_with_dims content_dims mat content_offset

    (*f layout_content_with_dims - OVERRIDE with layout widget - set actual draw dimensions for content
     *)
    method layout_content_with_dims (dims:float t_dims3) (mat:Matrix.t) (offset:float t_dims3) =
      ignore (self#set_layout dims mat offset); (* Dont use internal transform without killing offset? *)
      List.iter (fun w -> w#layout dims mat offset) children;
      ()

    (*f set_layout - set the matrix (and dimension/offset if clipping is to be performed) for later drawing *)
    method set_layout ldims lmat loffset =
      content_draw_dims <- ldims;
      ignore (Matrix.assign_m_m lmat (matrix_translate loffset) temp_matrix);
      ignore (Matrix.assign_m_m temp_matrix content_transformation internal_transformation);
      internal_transformation

    (*f draw - draw the widget, decoration border first, then content, children, and planes *)
    method draw app projection =
      (* draw decoration *)
      decoration#draw_border app projection;
      (* draw content *)
      self#draw_content app projection internal_transformation;
      (* draw children *)
      List.iter (fun c -> c#draw app projection) children;
      (* draw background *)
      decoration#draw_background app projection

    (*f get_content_desired_dims - get the xyz dimensions of the size of the content of the widget (override if a specific widget) *)
    method get_content_desired_dims = [|0.;0.;0.|]

    (*f get_content_draw_dims - get the xyz dimensions of the drawing size of the content of the widget, post-layout, use by subclasses *)
    method get_content_draw_dims = content_draw_dims

    (*f draw_content - draw the actual content of the widget (override if a specific widget) *)
    method draw_content     app projection transformation = () (* Draw content *)

    (*f key - handle a keypress along the action vector *)
    method key action k meta vector =
      List.iter (fun c -> ignore (c#key action k meta vector)) children;
      None

    (*f intersect_ray -  *)
    method intersect_ray cr =
      Collider_ray.rect_intersect ~transform_i:bbox_transformation_i cr bbox_draw_offset content_draw_dims

    (*f mouse - handle a mouse action along the action vector *)
    method mouse action mouse vector options =
      let (cr,max_d) = vector in
      let opt_intersection = self#intersect_ray cr in
      match opt_intersection with
        None -> None
      | Some k -> 
         if (k>max_d) then
           None
         else
           let thing acc_k c = 
             let (k, opt_cb) = acc_k in
             match (c#mouse action mouse (cr,k) options) with
               None -> acc_k
             | Some k_cb -> 
                let (k, cb) = k_cb in
                (k, Some cb)
           in
           let (k, opt_cb) = List.fold_left thing (max_d, None) children in
           if (option_is_none opt_cb) then
             None
           else
             Some (k, option_get opt_cb)

    (*f request_redraw *)
    method request_redraw = 
      match parent with
        Some widget -> widget#request_redraw
      | None -> ()

    (*f str *)
    method str = 
      sfmt "id:%s" id
  (*f All done *)
  end

(*c ogl_widget_box  *)
class ogl_widget_box stylesheet name_values =
  object (self)
    inherit ogl_widget stylesheet stylable_widget_box_desc "box" name_values  as super
  end

(*c ogl_widget_grid  *)
exception Bad_grid_layout of string
class ogl_widget_grid stylesheet name_values =
 
  object (self)
  val mutable grid_growth : (float t_dims3) option = None
  val mutable grid_shrink : (float t_dims3) option = None
  val mutable grid_base : (int t_dims3) option     = None
  val mutable grid_span : (int t_dims3)            = [|1;1;1;|]
  val mutable placement : (float t_dims3) option   = None
  val spans = [|Span.create (); Span.create (); Span.create (); |]

    inherit ogl_widget stylesheet stylable_widget_grid_desc "grid" name_values  as super

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

(*c ogl_widget_text  *)
class ogl_widget_text stylesheet name_values =
  object (self)
    inherit ogl_widget stylesheet stylable_widget_text_desc "text" name_values   as super
    val mutable text = "not banana";
    val mutable font_size_ref       = svr_zero;
    val mutable font_height_ref     = svr_zero;
    val mutable font_thickness_ref  = svr_zero;
    val mutable font_color_ref      = svr_zero;
    val mutable font_size           = 0.;
    val mutable font_height         = 0.;
    val mutable font_thickness      = 0.; (* Currently not supported by ogl_obj_text *)
    val mutable font_color  = Animatable_linear_float.create [|0.;0.;0.;|]
    val mutable font = font_default;
    val mutable text_dims = [|0.;0.;0.|];
    val mutable obj = None;

  method create app =
    super#create app ;
    font_color_ref     <- Stylable.get_value_ref self#get_stylable "font_color" ;
    font_size_ref      <- Stylable.get_value_ref self#get_stylable "font_size" ;
    font_height_ref    <- Stylable.get_value_ref self#get_stylable "font_height" ;
    font_thickness_ref <- Stylable.get_value_ref self#get_stylable "font_thickness" ;
    font_size          <- Stylable_value_ref.get_value_as_float font_size_ref;
    font_height        <- Stylable_value_ref.get_value_as_float font_height_ref;
    font_thickness     <- Stylable_value_ref.get_value_as_float font_thickness_ref;
    Animatable_linear_float.set_value font_color (Stylable_value_ref.get_value_as_floats font_color_ref);
    self#set_text text;
    Ok ()

  method set_text ?font_to_set text_to_set = 
    if (not self#can_create) then begin
        text <- text_to_set;
        Ok ()
    end
    else begin
        if (option_is_some obj) then (ignore ((option_get obj)#delete_geometry); obj <- None);
        if (option_is_some font_to_set) then font <- (option_get font_to_set);
        let fsize = font_size in
        let fheight = max font_size font_height in
        text <- text_to_set;
        let text_obj = new ogl_obj_text ~size:fsize ~height:fheight font text in
        let (x0,x1,y0,y1,z0,z1) = text_obj#get_bbox in
        text_dims <- [|x1 -. x0; y1 -. y0; z1 -. z0|];
        text_obj#create_geometry ~offset:(((x0 +. x1) *. (-. 0.5)), ((y0 +. y1) *. (-. 0.5)), 0.) >>=
          fun _ ->
          ( obj <- Some text_obj; Ok () )
    end
  method get_content_desired_dims = 
    text_dims
  method draw_content app projection transformation =
    if (option_is_none obj) then ()
    else
      (let program = app#get_program "widget_color" in
      let color_uid  = Gl_program.get_uniform_id "C"   program in
      let g_uid      = Gl_program.get_uniform_id "G"   program in
      let p_uid      = Gl_program.get_uniform_id "P"   program in
      Gl.use_program program.Gl_program.prog_id;
      let rgb = Animatable_linear_float.get_value font_color in
      Gl.uniform3f color_uid rgb.(0) rgb.(1) rgb.(2);
      Gl.uniform_matrix4fv g_uid 1 true (ba_of_matrix4 transformation);
      Gl.uniform_matrix4fv p_uid 1 true projection;
      (option_get obj)#draw;
      Gl.bind_vertex_array 0;
      ())

    (*f style_change *)
    method style_change sid_svs =
      if (self#can_create) then (
        Animatable_linear_float.set_value font_color (Stylable_value_ref.get_value_as_floats font_color_ref);
        super#style_change sid_svs;
      );
      ()

    (*f mouse - handle a mouse action along the action vector *)
    val mutable mouse_state = 0;
    method mouse_action action mouse vector options =
      let (cr,max_d) = vector in
      let opt_k = self#intersect_ray cr in
      match mouse_state with
        0 -> (if ((action<>Mouse_action_motion) or (option_is_none opt_k)) then McbNone else
                (mouse_state <- 1;Stylable.set_element_state 0 2 self#get_stylable;Stylesheet.apply_stylesheet stylesheet;McbSome (self#mouse_action))
                 )
       | 1 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Stylable.set_element_state 0 1 self#get_stylable;Stylesheet.apply_stylesheet stylesheet;McbNone)
            else if (action=Mouse_action_down) then
             (mouse_state <- 2;Stylable.set_element_state 0 3 self#get_stylable;Stylesheet.apply_stylesheet stylesheet;McbSome (self#mouse_action))
            else (McbSome (self#mouse_action))
           )
       | 2 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Stylable.set_element_state 0 1 self#get_stylable;Stylesheet.apply_stylesheet stylesheet;McbNone)
            else if (action=Mouse_action_up) then
              (
                (self#get_app)#button_pressed (self:>t_ogl_widget);
                mouse_state <- 1;Stylable.set_element_state 0 2 self#get_stylable;Stylesheet.apply_stylesheet stylesheet;McbSome (self#mouse_action))
            else (McbSome (self#mouse_action))
           )
       | _ -> McbNone
    
    method mouse action mouse vector options =
      let (cr,max_d) = vector in
      match (self#intersect_ray cr) with
        None -> None
      | Some k -> Some (k, self#mouse_action)

    (*f All done *)
  end

(*c ogl_widget_viewer, an OpenGL ogl_obj list viewer  *)
let vector_x_axis = Atcflib.Vector.make3 1. 0. 0.
let vector_y_axis = Atcflib.Vector.make3 0. 1. 0.
let vector_z_axis = Atcflib.Vector.make3 0. 0. 1.
class ogl_widget_viewer stylesheet name_values  = 
  object (self)
    inherit ogl_widget stylesheet stylable_widget_viewer_desc "viewer" name_values  as super
  val keys_down = ref Intset.empty
  val direction = Quaternion.make_rijk 1.0 0. 0. 0.
  val scale   = ref 1.
  val centre_x  = ref 0.
  val centre_y  = ref 0.
  val centre_z  = ref 0.
  val mutable idler_handle = -1
  val mutable draw_fn = let d a t = () in d
  val rotation = Matrix.make 4 4
  val translation = Matrix.make 4 4
  val view = Matrix.make 4 4
  val tmp = Matrix.make 4 4
  val q1 = Quaternion.make ()
  val q2 = Quaternion.make ()
  val q3 = Quaternion.make ()
  val mutable opt_program = None
  val mutable objs:ogl_obj list = []

    (*f create *)
    method create app =
      super#create app ;
      let gl_program_desc = Gl_program.make_desc "shaders/vertex_obj_viewer.glsl" "shaders/fragment.glsl" [] ["M"; "V"; "G"; "P";] in
      app#add_program "p" gl_program_desc >>= fun _ ->
         let program = app#get_program "p" in
         let m_uid      = Gl_program.get_uniform_id "M"   program in
         let v_uid      = Gl_program.get_uniform_id "V"   program in
         let g_uid      = Gl_program.get_uniform_id "G"   program in
         let p_uid      = Gl_program.get_uniform_id "P"   program in
         opt_program <- Some (program, m_uid, v_uid, g_uid, p_uid);
         self#create_geometry;
         idler_handle <- app#add_idler self#idle ;
         Ok ()

    (*f create_geometry *)
    method create_geometry =
      List.iter (fun o -> ignore (o#create_geometry ~offset:(0.,0.,0.))) objs

    (*f delete_geometry *)
    method delete_geometry =
      List.iter (fun o -> ignore (o#delete_geometry)) objs

    (*f set_objs *)
    method set_objs o = 
      if (self#can_create) then self#delete_geometry;
      objs <- o;
      if (self#can_create) then self#create_geometry

    (*f draw_content *)
    method draw_content app projection transformation =
      if (option_is_none opt_program) then () else
      begin    
        let (program, m_uid, v_uid, g_uid, p_uid) = (option_get opt_program) in
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.identity translation);
        ignore (Matrix.set 0 3 (-. !centre_x) translation);
        ignore (Matrix.set 1 3 (-. !centre_y) translation);
        ignore (Matrix.set 2 3 (-. !centre_z) translation);
        ignore (Matrix.assign_m_m rotation translation tmp);
        let ar_scale = (min (super#get_content_draw_dims).(0) (super#get_content_draw_dims).(1)) *. 0.35 *. !scale in
        ignore (Matrix.scale ar_scale tmp);  (* Make -1/1 fit the width *)
        Gl.use_program program.Gl_program.prog_id;
        Gl.uniform_matrix4fv p_uid 1 true projection;
        Gl.uniform_matrix4fv g_uid 1 true (ba_of_matrix4 transformation);
        Gl.uniform_matrix4fv v_uid 1 true (ba_of_matrix4 tmp);
        Gl.uniform_matrix4fv m_uid 1 true identity4;
        List.iter (fun o -> o#draw) objs;
        Gl.bind_vertex_array 0;
      end

    (*f pitch *)
    method private pitch amount = 
      ignore (Quaternion.assign_of_rotation vector_x_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f yaw *)
    method private yaw amount = 
      ignore (Quaternion.assign_of_rotation vector_y_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f roll *)
    method private roll amount = 
      ignore (Quaternion.assign_of_rotation vector_z_axis (cos amount) (sin amount) q1);
      ignore (Quaternion.(postmultiply q1 direction))

    (*f move_forward *)
    method private move_forward scale = 
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.scale scale rotation);
        let z = Vector.coords (Matrix.row_vector rotation 2) in
        centre_x := !centre_x +. (z.(0)) ;
        centre_y := !centre_y +. (z.(1)) ;
        centre_z := !centre_z +. (z.(2))

    (*f idle *)
    method private idle _ = 
      if Intset.mem (int_of_char 'l') !keys_down then self#move_forward (-0.001);
      if Intset.mem (int_of_char ',') !keys_down then self#move_forward 0.001;
      if Intset.mem (int_of_char ';') !keys_down then self#pitch 0.005;
      if Intset.mem (int_of_char '.') !keys_down then self#pitch (-0.005);
      if Intset.mem (int_of_char 'a') !keys_down then self#yaw 0.005;
      if Intset.mem (int_of_char 's') !keys_down then self#yaw (-0.005);
      if Intset.mem (int_of_char 'z') !keys_down then self#roll 0.005;
      if Intset.mem (int_of_char 'x') !keys_down then self#roll (-0.005);
      if Intset.mem (int_of_char '\'') !keys_down then scale := !scale *. 1.05;
      if Intset.mem (int_of_char '/') !keys_down then  scale := !scale /. 1.05;
      if Intset.mem 27 !keys_down then None else
        (self#request_redraw ; Some 10)

    (*f key - handle a keypress along the action vector *)
    method key action k meta vector =
      (match action with
         Key_action_press -> (keys_down := Intset.add k !keys_down)
       | Key_action_release -> (keys_down := Intset.remove k !keys_down)
      );
      None

    (*f mouse - handle a mouse action along the action vector *)
    method mouse action mouse vector options =
      let (cr,max_d) = vector in
      match (self#intersect_ray cr) with
        None -> None
      | Some k -> Some (k, fun a m v o -> 
    Printf.printf "Mouse at %f %s\n%!" k (str_av v);
    McbSome (fun a m v o -> Printf.printf "Mouse claimant %f %s\n%!" k (str_av v); McbNone)
    )

    (*f All done *)
  end

(*c ogl_widget_display - Display which contains a single widget Should
  this own an OpenGL context and an openGL render buffer (which might
  be an OS window)?
  *)
let screen_ppmm   = 5. (* 5 pixels per mm *)
let screen_2ppmm  = 2. *. screen_ppmm
let screen_depth_mm = 100. (* 10cm deep in, 10 cm out *)

class ogl_widget_display stylesheet name_values (toplevel_init:t_ogl_widget option): t_ogl_display = 
object (self)
  inherit ogl_widget stylesheet stylable_widget_display_desc "display" name_values  as super
  val mutable toplevel_widget : t_ogl_widget option = toplevel_init
  val mutable app : t_ogl_app option = None
  val projection = Matrix.make 4 4
  val screen_to_mm = Matrix.make 4 4
  val tmp_vector       = Vector.make 4
  val action_launch    = Vector.make 4
  val action_direction = Vector.make 4
  val play1 = Matrix.make 4 4
  val play2 = Matrix.make 4 4
  val playq1 = Quaternion.make_rijk 1.0 0.0 0.0 0.
  val playq2 = Quaternion.make_rijk 1.0 0.0 0.0 0.
  initializer
    if option_is_some toplevel_init then
     super#add_child (option_get toplevel_init) ;
    ()
  method create app_init =
    Quaternion.assign_of_rotation (Vector.make3 0.1 0.9 0.1) (cos 0.003) (sin 0.003) playq2 ;
    app <- Some app_init ;
    super#create app_init
  method display_reshape w h = (* Probably wants to become a set_bbox *)
    let w_mm = (float w) /. screen_ppmm in
    let h_mm = (float h) /. screen_ppmm in
    ignore Matrix.(identity projection |>
              set 0 0 (screen_2ppmm /. (float w)) |>
              set 1 1 (screen_2ppmm /. (float h)) |>
              set 2 2 (1. /. screen_depth_mm)) ;
    ignore Matrix.(identity screen_to_mm |>
              set 0 0 (1.0 /. screen_ppmm) |>
              set 0 3 (w_mm /. (-. 2.0)) |>
              set 1 1 (1.0 /. (-. screen_ppmm)) |>
              set 1 3 (h_mm /. 2.0) |>
              set 2 2 (1.)) ;
    let ddims = self#get_desired_dims in (* Propagates to children *)
    Printf.printf "Display desired dimensions %s\n" (str_fa ddims);
    self#layout [|w_mm; h_mm; screen_depth_mm|] Matrix.(identity (make 4 4)) [|0.0;0.0;0.|];
    Gl.viewport 0 0 w h ;
    ()
  method display_draw = 
    Gl.enable Gl.depth_test;
    Gl.clear_color 0.5 0.5 0.5 1.;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    Matrix.identity play1;
    Quaternion.premultiply playq2 playq1;
    Matrix.assign_from_q playq1 play1;
    Matrix.assign_m_m projection play1 play2;
    if true then
      self#draw (option_get app) (ba_of_matrix4 projection)
    else
      self#draw (option_get app) (ba_of_matrix4 play2)
  method private initial_action_vector x y =
    ignore (Vector.(set 0 (float x) tmp_vector |>
                      set 1 (float y) |>
                      set 2 (-. screen_depth_mm) |>
                      set 3 1.0));
    ignore (Vector.assign_m_v screen_to_mm tmp_vector action_launch);
    ignore (Vector.(set 0 0. action_direction |>
                      set 1 0. |>
                      set 2 (2. *. screen_depth_mm) |>
                      set 3 0.0));
    ((Collider_ray.create action_launch action_direction), 1.0)

  (*f display_key
    Propagate through children along the 'action_vector' - effectively where the key was pressed
    Of course a focus window could be set up to be the actual claimant of a key press

    The resulting callback of the propagation (key_result) can then be invoked

    The result of the callback could be a claim on future keypresses or somesuch
   *)
  method display_key     action k m x y =
    let action_vector = self#initial_action_vector x y in
    let key_result = self#key action k m action_vector in
    match key_result with
      Some da -> let (_,cb) = da in cb action k m action_vector
    |  _ -> None

  (*f display_mouse
    Propagate through children along the 'action_vector' - effectively where the mouse event occurred

    The resulting callback of the propagation (mouse_result) can then be invoked

    The result of the callback could be a claim on future mouse events
   *)
  method display_mouse action mouse x y options =
    let action_vector = self#initial_action_vector x y in
    let mouse_result  = self#mouse action mouse action_vector options in
    match mouse_result with
      Some da -> let (_,cb) = da in cb action mouse action_vector options
    |  _ -> McbNone

  (*f display_mouse_claimant
    The result of the callback could be a claim on future mouse events
   *)
  method display_mouse_claimant action mouse x y options callback =
    let action_vector = self#initial_action_vector x y in
    callback action mouse action_vector options

    method request_redraw = 
      match super#get_parent with
        Some widget -> widget#request_redraw
      | None -> (option_get app)#request_redraw (self:>t_ogl_display)
end

(*a OpenGL app virtual class and basic apps *)
(*c ogl_app *)
class ogl_app stylesheet ogl_displays : t_ogl_app =
object (self)
  (*b Properties *)
  val mutable program_list:(string*Gl_program.t) list = [];
  val mutable display_list:(string*t_ogl_display*t_window_handle) list = [];
  val mutable next_handle = 0;
  val mutable create_window_fn = fun ~width ~height ~title handle -> Ok 0;
  val mutable window_displays : (t_window_handle * t_ogl_display) list = [];
  val mutable should_quit  = false;
  val mutable idler_list:(int*(unit -> int option)) list = [];
  val mutable opt_mouse_claimant : (t_ogl_display * t_mouse_claimant) option = None;

  (*f set_create_window *)
  method set_create_window cwin =
    create_window_fn <- cwin

  (*f get_next_handle - returns a monotonically increasing handle value *)
  method private get_next_handle =
    next_handle <- next_handle + 1;
    next_handle

  (*f create_window - call OS callback to create a window of certain width/height/title with a toplevel ogl_display, return a window handle *)
  method create_window ?width:(width=0) ?height:(height=0) ?title:(title="") display = 
    let handle = self#get_next_handle in
    let wh = create_window_fn ~width:width ~height:height ~title:title handle in
    window_displays <- window_displays @ [(next_handle, display)];
    wh

  (*f display_of_handle - find ogl_display from a window handle to be returned to the OS side *)
  method private display_of_handle (handle:t_window_handle) =
    if (not (List.mem_assoc handle window_displays)) then None else Some (List.assoc handle window_displays)

  (*f add_display - add a name/ogl_display/window_handle to the app *)
  method add_display (name:string) display window_handle = 
    (display_list <- (name,display,window_handle)::display_list )


  (*f create - create the app *)
  method create = 
    let do_all_displays f =
      let do_if_ok acc od =
        match acc with
          Error _ as e -> e
        | Ok hd -> 
           (
             match f od with
               Error _ as e -> e
             | Ok r -> Ok (hd @ [r])
           )
      in
      List.fold_left do_if_ok (Ok []) ogl_displays
    in
    do_all_displays (fun od -> self#create_window ~width:800 ~height:600 ~title:"Display" od)
    >>= fun window_handles ->
    raise_any_error (self#add_program "widget_color" (Gl_program.make_desc "shaders/widget_vertex.glsl" "shaders/widget_color_fragment.glsl" [] ["G"; "P"; "C"]))
    >>= fun _ ->
    raise_any_error (self#add_program "standard_color" (Gl_program.make_desc "shaders/vertex_standard.glsl" "shaders/fragment_color.glsl" [] ["M"; "V" ; "G"; "P"; "C"]))
    >>= fun _ ->
    do_all_displays (fun od -> od#create_tree_styles)
    >>= fun _ ->
    ignore (Stylesheet.build stylesheet (List.map (fun od -> od#get_stylable) ogl_displays));
    Stylesheet.apply_stylesheet stylesheet;
    do_all_displays (fun od -> od#create (self:>ogl_app))
    >>= fun _ ->
    List.iteri (fun i d -> self#add_display (sfmt "toplevel%d" i) d (List.nth window_handles i)) ogl_displays;
    Ok ()

  (*f add_program name program_desc - add an Gl_program.t OpenGL shader program (vertex + fragment) to the app *)
  method add_program (name:string) program_desc = 
    raise_any_error (Gl_program.make program_desc) >>=
    fun p -> (program_list <- (name,p)::program_list ; Ok () )

  (*f get_program name - get the Gl_program.t of a named program that has been added *)
  method get_program name = List.assoc name program_list

  (*f draw window_handle - draw the ogl_display corresponding to the window handle *)
  method draw handle =
    match self#display_of_handle handle with
      Some display -> display#display_draw
    | None -> ()

  (*f reshape window_handle - resize the ogl_display corresponding to the window handle *)
  method reshape handle w h =
    match self#display_of_handle handle with
      Some display -> display#display_reshape w h
    | None -> ()

  (*f key window_handle - handle a keypress in the appropriate window, or none if none has focus *)
  method key opt_handle action k meta x y =
    let invoke_display_callback opt_handle callback =
      if (option_is_none opt_handle) then None else
        let handle = option_get opt_handle in
        match self#display_of_handle handle with
          Some display -> callback display
        | None -> None
    in
    if (k=27) then should_quit<-true ;
    ignore (invoke_display_callback opt_handle (fun display -> display#display_key action k meta x y))

  (*f mouse window_handle - handle a keypress in the appropriate window, or none if none has focus *)
  method mouse opt_handle action mouse x y options =
    let invoke_display_callback opt_handle callback =
      if (option_is_none opt_handle) then None else
        let handle = option_get opt_handle in
        match self#display_of_handle handle with
          Some display -> (
          match (callback display) with
            McbNone -> None
          | McbSome cb -> Some (display,cb)
        )
        | None -> None
    in
    if (option_is_some opt_mouse_claimant) then (
      opt_mouse_claimant <-
        let (display, cb)=option_get opt_mouse_claimant in
        match (display#display_mouse_claimant action mouse x y options cb) with
          McbNone -> None
         | McbSome cb -> Some (display,cb)
    );
    if (option_is_none opt_mouse_claimant) then (
      opt_mouse_claimant <- invoke_display_callback opt_handle (fun display -> display#display_mouse action mouse x y options)
    )

  (*f destroy the application - delete programs, ogl_displays, etc *)
  method destroy : unit ogl_result = 
    let delprog n_p = ignore (Gl_program.delete (snd n_p)); () in
    let deldisp n_d = let (a,b,c)=n_d in b#destroy in
    List.iter delprog program_list ;
    List.iter deldisp display_list ;
    Ok ()

  (*f request_redraw - a display requests redraw before next idle *)
  method request_redraw display = 
    ()

  (*f need_redraw - return list of window handles (of displays) needing redraw *)
  method need_redraw = 
    let (_,_,wh) = List.nth display_list 0 in
    [wh]

  (*f button_pressed *)
  method button_pressed widget = ()

  (*f add_idler - add a callback to be invoked on idle *)
  method add_idler idler_fn =
    let handle = self#get_next_handle in
    idler_list <- idler_list @ [(handle,idler_fn)] ;
    handle

  (*f idle - return Some <delay in msec> or None (to quit) *)
  method idle = (* Idles here *)
    if should_quit then None
    else
      ( let check_idle earliest h_i =
          let (handle,idler_fn) = h_i in
          match idler_fn () with
            None -> None
          | Some d -> if (option_is_none earliest) then None else if ((option_get earliest)<d) then earliest else (Some d)
        in
        List.fold_left check_idle (Some 10000000) idler_list )
end


(*a App_build *)
module App_build = struct
  type stack_entry_element = ((string * string) list * (ogl_widget option))
  type stack_entry         = stack_entry_element list
  type t = 
    {
      app_creator : (ogl_widget_display list) -> ogl_app;
      stylesheet : Stylesheet.t;
      xml_additions : (string * (t -> string -> (string * string) list -> unit) ) list;
      mutable widget_stack : (string * Sax.attribute list) list;
      mutable children_stack : stack_entry list;
      mutable current_children : stack_entry;
      mutable displays : (ogl_widget_display list);
      mutable app : ogl_app option;
    }
  let create app_creator stylesheet xml_additions= {app_creator; stylesheet; xml_additions; widget_stack=[]; children_stack=[]; current_children=[]; displays=[]; app=None}

  let list_head_tail l = 
    match l with
      [] -> (Printf.printf "List empty\n%!"; raise Not_found)
    | hd::tail -> (hd, tail)

  let push_widget t name atts=
    t.widget_stack <- (name, atts)::t.widget_stack;
    t.children_stack <- t.current_children::t.children_stack;
    t.current_children <- [];
    ()

  let add_child t widget =
    t.current_children <- ([], Some widget)::t.current_children;
    ()

  let add_grid_element t name_values child =
    t.current_children <- (name_values, Some child)::t.current_children;
    ()

  let add_grid_span t name_values =
    t.current_children <- (name_values, None)::t.current_children;
    ()

  let iter_children f children =
    List.iter (fun nvs_w ->
                let (_,opt_w)=nvs_w in
                if option_is_some opt_w then
                  let w = option_get opt_w in
                  f w
              )
              children

  let children_nth_widget n children =
    if ((List.length children)<=n) then
      None
    else
      let (_,opt_w) = List.nth children n in
      opt_w

  let pop_widget t =
    let children = t.current_children in
    let (parents_children,rest_children) = list_head_tail t.children_stack in
    let ((name,atts),rest_widgets)       = list_head_tail t.widget_stack in
    t.children_stack <- rest_children;
    t.widget_stack <- rest_widgets;
    t.current_children <- parents_children;
    (name, atts, children)

  let make_app t children =
    t.app <- Some (t.app_creator t.displays);
    ()

  let add_display t display =
    display#set_depth 0;
    t.displays <- t.displays@[display];
    ()

  let get_opt_app t = 
    t.app

let start_element app ~uri ~localName ~qName ~atts =
  push_widget app localName atts;
  ()

let end_element app ~uri ~localName ~qName =
  let (st_name, atts, children) = pop_widget app in
  let name_values = Sax.Attributes.nameValues atts in
  if (List.mem_assoc st_name app.xml_additions) then (
    (List.assoc st_name app.xml_additions) app st_name name_values
  )
  else
    (
      match st_name with
        "label" -> 
        (
          let text     = (Sax.Attributes.getValue ~default:""   atts "text") in
          let widget = new ogl_widget_text app.stylesheet name_values in
          widget#name_value_args name_values;
          ignore (widget#set_text text);
          add_child app (widget :> ogl_widget)
        )
      | "window" -> 
         (
           let display = new ogl_widget_display app.stylesheet name_values None in
           display#name_value_args name_values;
           iter_children (fun w -> display#add_child w) children;
           add_display app display
         )
      | "box" -> 
         (
           let widget = new ogl_widget_box app.stylesheet name_values in
           widget#name_value_args name_values;
           iter_children (fun w -> widget#add_child w) children;
           add_child app widget
         )
      | "grid_element" -> 
         (
           let opt_w = children_nth_widget 0 children in
           if (option_is_some opt_w) then
             add_grid_element app name_values (option_get opt_w)
         )
      | "grid_span" -> 
         (
           let grid_span = (name_values) in
           add_grid_span app grid_span
         )
      | "grid" -> 
         (
           let widget = new ogl_widget_grid app.stylesheet name_values in
           widget#name_value_args name_values;
           let element_or_span_data nvs_ow =
             let (nvs,opt_w) = nvs_ow in
             if option_is_some opt_w then
               let w = option_get opt_w in
               (widget#set_element nvs w)
             else
               (widget#set_span_data nvs)
           in
           List.iter element_or_span_data children;
           widget#grid_build;
           add_child app (widget :> ogl_widget)
         )
      | "app" -> 
         (
           make_app app children
         )
      | _ -> ()
    )

open Sax
let create_app_from_xml app_xml stylesheet xml_additions app_creator = 
  let app = create app_creator stylesheet xml_additions in
  let ogl_sax_content_handler =
    Content_handler.create
      ~start_element_callback:start_element
      ~end_element_callback:end_element
      ~callback_state:app
      ()
  in
  let ogl_sax_parser = Sax.XMLReader.create ~content_handler:ogl_sax_content_handler () in
  match Sax.XMLReader.parse_string ogl_sax_parser app_xml with
      Error msg -> ( Printf.printf "%s\n" msg; None )
    | Ok ()     -> ( get_opt_app app)

end

