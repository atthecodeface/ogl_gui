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
 * @file          plot_ogl.ml
 * @brief         Plot graphs using OpenGL
 *
 *)

(*a Libraries *)
open Atcflib
open Tgl4
open Ogl
open Sdl_ogl
open Stylesheet

(*a Obj-file ogl_obj *)
(*c ogl_obj_file - Object from an object filename
 obj format supported

v <float> [<floats>*]
Vertex with x, y, z, w - w is 1 if not provided, other coordinates 0, up to 4 coordinates

vn <float> [<floats>*]
Normal with x, y, z - coordinates 0 if not provided, up to 3 coordinates

vp <float> [<floats>*]
Parameters with u, v, w - coordinates 0 if not provided, up to 3 coordinates

vt <float> [<floats>*]
Texture coordinate with u, v, w - coordinates 0 if not provided, up to 3 coordinates

f [indicies]+

indices = <vertex index>[/[<optional texture index>][/<optional normal index>]]

mtllib [external .mtl file name]
usemtl [material name]

o [object name]
g [group name]

Smooth shading across polygons is enabled by smoothing groups.

s 1
  ...
  # Smooth shading can be disabled as well.
  s off
  ...

Relative and absolute indices

OBJ files, due to their list structure, are able to reference vertices, normals, etc. either by their absolute position (1 represents the first defined vertex, N representing the Nth defined vertex), or by their relative position (-1 represents the latest defined vertex). However, not all software supports the latter approach, and conversely some software inherently writes only the latter form (due to the convenience of appending elements without needing to recalculate vertex offsets, etc.), leading to occasional incompatibilities.

*)

let float_rex =
  let opt_whitespace = Re.rep Re.space in
  let point = Re.char '.' in
  let minus = Re.char '-' in
  let digit = Re.set "0123456789" in
  Re.seq [ Re.opt_whitespace ; Re.opt minus; Re.rep1 digit ; Re.opt point ; Re.rep digit]

let int_rex =
  let opt_whitespace = Re.rep Re.space in
  let minus = Re.char '-' in
  let digit = Re.set "0123456789" in
  Re.seq [ Re.opt_whitespace ; Re.opt minus; Re.rep1 digit]

let vertex_type_rex =
  let point = Re.char '.' in
  let minus = Re.char '-' in
  let digit = Re.set "0123456789" in
  let whitespace = Re.rep1 Re.space in
  Re.seq [ Re.group (Re.seq [ Re.char 'v'; Re.opt (Re.set "tn");]) ;
           Re.group float_rex ;
           Re.group (Re.opt float_rex) ;
           Re.group (Re.opt float_rex) ;
           Re.group (Re.opt float_rex) ;
         ]

let face_indices_rex =
  let whitespace     = Re.rep1 Re.space in
  let opt_whitespace = Re.rep Re.space in
  let opt_int = Re.opt (Re.group int_rex) in
  Re.seq [ opt_whitespace ; Re.group int_rex ;
           Re.opt (Re.seq [opt_whitespace ;
                           Re.char '/' ;
                           opt_int;
                           Re.opt (Re.seq [opt_whitespace ;
                                           Re.char '/' ;
                                           opt_int;])
                          ])
         ]

let face_type_rex =
  let whitespace = Re.rep1 Re.space in
  Re.seq [ Re.char 'f'; whitespace;
           Re.group (Re.rep1 face_indices_rex) ]

module obj_file = struct
  module face = struct
    type t = {
      indices = (int * int * int) list
      }
    let create _ = { indices = [] )
    let add_v   t v       = t.indices <- t.indices @ [ (v,0,0) ]
    let add_vt  t v vt    = t.indices <- t.indices @ [ (v,vt,0) ]
    let add_vtn t v vt vn = t.indices <- t.indices @ [ (v,vt,vn) ]
    let add_vn  t v vn    = t.indices <- t.indices @ [ (v,0,vn) ]
  end
  type t = {
    vertices  : float32_bigarray;
    normals   : float32_bigarray;
    textures  : float32_bigarray;
    faces     : int32_bigarray;
    }
  type t2 = {
    mutable r_vertex_list  : float list; (* Reverse normal list *)
    mutable r_normal_list  : float list; (* Reverse normal list *)
    mutable r_texture_list : float list; (* Reverse normal list *)
    mutable faces : face.t list; (* NOT reversed list *)
    }
  create _ =
    { vertices = Bigarray.(Array1.create float32 c_layout 0);
      normals  = Bigarray.(Array1.create float32 c_layout 0);
      textures = Bigarray.(Array1.create float32 c_layout 0);
      faces    = Bigarray.(Array1.create int32 c_layout 0);
    }
  let from_t2 t2 =
    let nv  = List.length t2.r_vertex_list in
    let nvt = List.length t2.r_texture_list in
    let nvn = List.length t2.r_normal_list in
    let nf  = List.length t2.faces in
    let t = 
    { vertices = Bigarray.(Array1.create float32 c_layout (nv  * 4));
      normals  = Bigarray.(Array1.create float32 c_layout (nvn * 3));
      textures = Bigarray.(Array1.create float32 c_layout (nvt * 3));
      faces    = Bigarray.(Array1.create int32 c_layout   (nf  * 3));
    }

  let add_vertex t2 x y z w =
    t2.r_vertex_list <- (x.y,z,w)::t2.r_vertex_list
  let add_normal t2 x y z =
    t2.r_normal_list <- (x.y,z)::t2.r_normal_list
  let add_texture t2 u v w =
    t2.r_texture_list <- (u,v,w)::t2.r_texture_list
  let add_face t2 = 
    let f = face.create ()
    in
    t2.faces <- t2.faces @ [f];
    f
  let face_add_v t2 f v = 
    face.add_v f v
  let face_add_vt t2 f v vt = 
    face.add_vt f v vt
  let face_add_vn t2 f v vn = 
    face.add_vn f v vn
  let face_add_vtn t2 f v vt vn = 
    face.add_vtn f v vt vn
  let load_file f =
    let t2 = {r_vertex_list = []; r_normal_list=[]; r_texture_list=[]; faces=[]} in
    let rec read_line n =
      try (
        let l = input_line f in
             match (Re.exec_opt msec_rex s) with
               Some g -> Int64.of_string(Re.Group.get g 1)
             | None -> 0L
        let somehting = re match in
        if (option_is_some something) then
          (
    let defalut_value = 0. in
    let (a,b,c,d) = floats_of_something something default_value in
    if something_else then (add_vertex t2 a b c d)
    else if something_else then (add_normal t2 a b c)
    else if something_else then (add_texture t2 a b c)
    else ()
          );
        else
          (
        let somehting = re match in
        if (option_is_some something) then
          (
    let f = add_face t2 in
    face_add_vtn t2 f v vt vn
          )
          );
          read_line (n+1)
      )
      with End_of_file -> ()
    in
    read_line 0;
    thing_of_t2 t2
    
end


(*a Application *)
(*f Load font to start with *)
open Font
let outline_font = Font.Outline.load_json "cabin-bold"

let sel_state_pressed   =  Stylable.is_element_state 0 3
let sel_state_hover   =  Stylable.is_element_state 0 2
let sel_state_enable  =  Stylable.is_element_state 0 1
let stylesheet = Ogl.create_stylesheet ()
let _ = 
    Stylesheet.add_style_rule stylesheet [Stylable.has_element_class "button"]
             [
             ("font_color", Sv_rgb [|0.6;0.6;0.6|]);
             ("face_color", Sv_rgb [|0.1;0.3;0.3|]);
             ("fill", Sv_int_3 [|1;1;1;|]);
             ("padding", Sv_float_6 [|1.;1.;1.;1.;1.;1.|]);
    ];
    Stylesheet.add_style_rule stylesheet [fun e-> (Stylable.has_element_class "button" e) && (sel_state_hover e)]
             [
             ("font_color", Sv_rgb [|0.6;0.7;0.6|]);
             ("face_color", Sv_rgb [|0.3;0.5;0.5|]);
    ];
    Stylesheet.add_style_rule stylesheet [fun e-> (Stylable.has_element_class "button" e) && (sel_state_pressed e)]
             [
             ("font_color", Sv_rgb [|0.8;0.7;0.6|]);
             ("face_color", Sv_rgb [|0.3;0.5;0.5|]);
    ];
    Stylesheet.add_style_rule stylesheet [Stylable.has_element_class "heading"]
             [("font_size", Sv_float 4.0);
             ("font_color", Sv_rgb [|0.7;0.5;0.3|])
    ];
    ()
let data = [(0.,0.); (0.1,0.1)]
class ogl_obj_data =
    object (self)
      inherit Ogl.ogl_obj as super
      val mutable num_pts = -1;
      method create_geometry ~offset =
        let ba_float_array len = Bigarray.(Array1.create float32 c_layout len) in
        let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len) in
        num_pts <- List.length data;
        let axis_vertices = ba_float_array (num_pts * 3) in
        let axis_colors   = ba_float_array (num_pts * 3) in
        let axis_indices  = ba_uint16_array (num_pts*2) in
        let do_pt i az =
          let sc = 0.6 +. 0.4 *. ((float i) /. (float num_pts)) in
          let (angle, z) = az in
          let x = -. sc *. cos (2. *. 3.14159265 *. angle) in
          let y = sc *. sin (2. *. 3.14159265 *. angle) in
          axis_indices.{2*i+0} <- i ;
          axis_indices.{2*i+1} <- (i+1) ;
          if true then begin
              axis_vertices.{i*3+0} <- x;
              axis_vertices.{i*3+1} <- z*.2.-.1. ;
              axis_vertices.{i*3+2} <- y ;
            end
          else begin
              axis_vertices.{i*3+0} <- 2.*. angle -. 1. ;
              axis_vertices.{i*3+1} <- z*.2.-.1. ;
              axis_vertices.{i*3+2} <- 0. ;
            end;
          axis_colors.{i*3+0} <- z ;
          axis_colors.{i*3+1} <- 0. ;
          axis_colors.{i*3+2} <- 1. -. z in
        List.iteri do_pt data ;
        super#create_geometry_from_indices axis_indices [axis_vertices; axis_colors]
      method draw =
        let d _ = 
           Gl.draw_elements Gl.lines (num_pts*2) Gl.unsigned_short (`Offset 0);
           ()
        in self#draw_internal d
    end

let app_xml = "<?xml?><app>
<window dims='100,100,100' fill='3,3,3'>
  <grid fill='3,3,3' id='main_grid'>
    <grid_span axis='x' weights='1.0,0.0,0.0'/>
    <grid_span axis='y' weights='1.0,0'/>
    <grid_span axis='z'/>
    <grid_element base='0,1,0' span='2,1,1'>
      <label text='Object viewer' font_size='5' align='0,0,0' border='1' border_color='0.5' fill='3,0,0'/>
    </grid_element>
    <grid_element base='0,0,0'>
      <plot_ogl dims='50,50,100' fill='3,3,3' align='0,0,0' border='1,1,1,1,1,1' border_color='0.1,0.1,0.1' id='viewer'/>
    </grid_element>
    <grid_element base='1,0,0'>
      <grid border='1,1,1,1,1,1' align='0,1,0' border_color='0.7,0.7,0.3' font_size='3' padding='1,1,1' id='cbox_grid' face_color='0.1,0.3,0.4'>
        <grid_element base='0,0,0'>
          <label text='Controls' class='ctl_heading heading' diams='30,10,0.2' id='lbl_controls'/>
        </grid_element>
        <grid_element base='0,-1,0'>
          <label text='Reload' class='button' id='reload'/>
        </grid_element>
      </grid>
    </grid_element>
  </grid>
</window>
</app>"

class ogl_widget_plot stylesheet name_values =
  object (self)
    inherit ogl_widget_viewer stylesheet name_values as super
    (*f mouse - handle a mouse action along the action vector *)
    method mouse action mouse vector options = None
end

class ogl_app_plot stylesheet ogl_displays : t_ogl_app = 
  object (self)
    inherit ogl_app stylesheet ogl_displays as super

  (*f button_pressed *)
  method button_pressed widget =
    Printf.printf "Button pressed %s\n%!" (widget#get_id);
     ()
end
    

let xml_additions = 
[
("plot_ogl", fun app _ name_values ->
    (
      let axes = new ogl_obj_geometry
                     Gl.lines 6 
                     [| 0; 1; 0; 2; 0; 3; |] (* indices *)
                     [ ba_floats [| 0.; 0.; 0.;
                        1.; 0.; 0.;
                        0.; 1.; 0.;
                        0.; 0.; 1.;|]; (* vertices *)
                     ba_floats [|1.0; 1.0; 1.0;
                       1.0; 0.0; 0.0;
                       0.0; 1.0; 0.0;
                       0.0; 0.0; 1.0;|];] (* 'colors' *)
      in
      let objs = [axes; new ogl_obj_data; ] in
      let widget = new ogl_widget_plot app.Ogl.App_build.stylesheet name_values in
      widget#set_objs objs;
      widget#name_value_args name_values;
      Ogl.App_build.add_child app (widget :> ogl_widget)
    ))
]

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf "Usage: %s [OPTION]\nPlots something\nOptions:" exec in
  let options =
    [ ]
  in
  let anon _ = raise (Arg.Bad "no arguments are supported") in
  Arg.parse (Arg.align options) anon usage;
  match (Ogl.App_build.create_app_from_xml app_xml stylesheet xml_additions (fun displays -> (new ogl_app_plot stylesheet displays))) with
    None -> 
    (
      Printf.printf "Failed to create app\n"; exit 1
    )
  | Some app ->
     (
       match (Sdl_ogl.run_app app) with
         Ok () -> exit 0
       | Error msg -> Printf.printf "%s\n" msg; exit 1
     )

let () = main ()

