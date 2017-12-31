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

open Stylesheet
open Utils
open Ogl_types
open Ogl_obj
open Ogl_program
open Ogl_obj_standard
open Ogl_widget (* for create_stylesheet *)
open Sdl_ogl_gui
open Atcflib
open Tgl4

(*f Load font to start with *)
let sel_true          =  (fun e -> true)
let sel_cbox          =  Stylable.is_element_id "control"
let sel_type_button   =  Stylable.is_element_type "text_button"
let sel_cls_rotate    =  Stylable.has_element_class "rotate"
let sel_state_pressed   =  Stylable.is_element_state 0 3
let sel_state_hover   =  Stylable.is_element_state 0 2
let sel_state_enable  =  Stylable.is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let stylesheet = Ogl_widget.create_stylesheet ()
let _ = 
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_hover_button]
             [("border_color", Sv_rgb [|1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_cbox; sel_type_button]
             [("border", Sv_float_6 [|1.;1.;1.;1.;1.;1.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [sel_true]
             [("margin", Sv_float_6 [|0.;0.;0.;0.;0.;0.;|]);
             ];
    Stylesheet.add_style_rule stylesheet [Stylable.has_element_class "button"]
             [
             ("font_color", Sv_rgb [|0.6;0.6;0.6|]);
             ("face_color", Sv_rgb [|0.1;0.3;0.3|]);
             ("fill", Sv_int_3 [|1;1;1;|]);
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
             [("font_size", Sv_float 8.0);
             ("font_color", Sv_rgb [|0.7;0.5;0.3|])
    ];
    ()


let ba_float_array len = Bigarray.(Array1.create float32 c_layout len)
let ba_uint16_array  len = Bigarray.(Array1.create int16_unsigned c_layout len)
let num_pts = 8 (* cube *)
let num_faces = 6*2 (* cube *)
let axis_vertices  = ba_float_array (num_pts * 3)
let axis_normals   = ba_float_array (num_pts * 3)
let axis_colors    = ba_float_array (num_pts * 3)
let axis_indices   = ba_uint16_array (num_faces*3) 
let set_pt n d =
    let set i v =
      if (i<3) then (axis_vertices.{n*3+i} <- v)
      else (if (i<6) then (axis_normals.{n*3+i-3} <- v)
        else (axis_colors.{n*3+i-6} <- v))
    in
    List.iteri set d
let set_face n d =
    List.iteri (fun i v -> axis_indices.{n*3+i} <- v) d
let _ =
    set_pt 0 [ 1.;  1.;  -1.;   1.;  1.;  -1. ;  1.;0.5;0.5];
    set_pt 1 [ 1.; -1.;  -1.;   1.; -1.;  -1. ;  1.;0.5;0.5];
    set_pt 2 [-1.; -1.;  -1.;  -1.; -1.;  -1. ;  1.;0.5;0.5];
    set_pt 3 [-1.;  1.;  -1.;  -1.;  1.;  -1. ;  1.;0.5;0.5];
    set_pt 4 [ 1.;  1.;   1.;   1.;  1.;  1. ;  0.;0.5;0.5];
    set_pt 5 [ 1.; -1.;   1.;   1.; -1.;  1. ;  0.;0.5;0.5];
    set_pt 6 [-1.; -1.;   1.;  -1.; -1.;  1. ;  0.;0.5;0.5];
    set_pt 7 [-1.;  1.;   1.;  -1.;  1.;  1. ;  0.;0.5;0.5];
    set_face 0  [0; 1; 3];
    set_face 1  [1; 3; 2];
    set_face 2  [3; 2; 7];
    set_face 3  [2; 7; 6];
    set_face 4  [7; 6; 4];
    set_face 5  [6; 4; 5];
    set_face 6  [4; 5; 0];
    set_face 7  [5; 0; 1];

    set_face 0  [0; 1; 2];
    set_face 1  [1; 2; 5];
    set_face 2  [2; 5; 0];
    set_face 3  [5; 0; 4];
    set_face 4  [0; 4; 3];
    set_face 5  [4; 3; 5];
    set_face 6  [4; 5; 0];
    set_face 7  [5; 0; 1];
    set_face 8  [0; 1; 3];
    set_face 9  [1; 3; 2];


class ogl_obj_data =
    object (self)
      inherit Ogl_obj.ogl_obj as super
      method create_geometry ~offset =
        super#create_geometry_from_indices axis_indices [axis_vertices; axis_normals; axis_colors]
      method draw =
        let d _ = 
           Gl.draw_elements Gl.triangles (num_faces*4) Gl.unsigned_short (`Offset 0);
           ()
        in self#bind_and_draw d
    end

let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(* XML structure
<!DOCTYPE app SYSTEM "app.dtd">
<!ELEMENT app (window)*>
<!ELEMENT window (grid|box|label|plot_ogl)*>
<!ELEMENT grid (grid_span|grid_element)*>
<!ELEMENT grid_element (grid|label|plot_ogl)>
<!ELEMENT box (grid|box|label|plot_ogl)>
<!ELEMENT label EMPTY>
<!ELEMENT plot_ogl EMPTY>

<!ATTLIST window
    dims         CDATA "" float triple, force size in mm; not present -> use content
    align        CDATA "" float triple, -1.0/1.0; 0.0=center, -1.0=left/bottom/outof, 1.0=>right/top/into
    fill         CDATA "" 0/1/2/3 triple; 1/3 can expand to fill, 2/3 can shrink
    border       CDATA "" float hextuple, border size in mm (left x, rightx, ...)
    border_color CDATA "" float triple color
>
<!ATTLIST label
    dims, align, fill, border, border_color

    text CDATA ""
    font_size CDATA "" (NYI)
    font      CDATA "" (NYI)
>
<!ATTLIST plot_ogl
    dims, align, fill, border, border_color
>
<!ATTLIST grid
    dims, align, fill, border, border_color
>
<!ATTLIST grid_span
    axis x|y|z #REQUIRED
    weights
    weights_shrink
    weights_grow
>
<!ATTLIST grid_element
    base
    span
>
 *)

let app_xml = "<?xml?><app>
<window dims='100,100,100' fill='3,3,3' border='1,1,1,1,1,1' border_color='0.3,0.3,0.3' align='0,1,0'>
  <grid fill='3,3,3' border='1,1,1,1,1,1' border_color='0.7,0.3,0.7' id='main_grid'>
    <grid_span axis='x' weights='1.0,0.0,0.0'/>
    <grid_span axis='y' weights='1.0,0'/>
    <grid_span axis='z'/>
    <grid_element base='0,1,0' span='2,1,1'>
      <label text='Title goes here' font_size='15' align='0,0,0' border='3,3,3,3,3,3' border_color='0.5,0.1,0.1' fill='3,0,0'/>
    </grid_element>
    <grid_element base='0,0,0'>
      <plot_ogl dims='50,50,100' fill='3,3,3' align='0,0,0' border='1,1,1,1,1,1' border_color='0.1,0.1,0.1' id='viewer'/>
    </grid_element>
    <grid_element base='1,0,0'>
      <grid border='1,1,1,1,1,1' align='0,1,0' border_color='0.7,0.7,0.3' font_size='4' id='cbox_grid' face_color='0.1,0.3,0.4'>
        <grid_element base='0,0,0'>
          <label text='Controls' class='ctl_heading heading' dims='30,10,0.2' id='lbl_controls' face_color='0.5,0.3,0.5'/>
        </grid_element>
        <grid_element base='0,-1,0'>
          <label text='More' class='button' id='but_more'/>
        </grid_element>
        <grid_element base='0,-2,0'>
          <label text='More2' class='button' id='but_more2'/>
        </grid_element>
        <grid_element base='0,-3,0'>
          <label text='Size The box' class='button' align='-1,0,0' id='lbl_size_the_box'/>
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
    method create app =
      opt_material <- Some (app#get_material "vnc_vertex") ;
      super#create app

    method mouse action mouse vector options = None
end

class ogl_app_plot stylesheet ogl_displays : t_ogl_app = 
  object (self)
    inherit Ogl_app.ogl_app stylesheet ogl_displays as super
    method create_shaders =
      super#create_shaders ;
      let gl_program_desc = Gl_program.make_desc "shaders/vnc_vertex.glsl" "shaders/fragment.glsl" [] ["M"; "V"; "G"; "P";] in
      self#add_program "vnc_vertex" gl_program_desc >>= fun _ ->
      Ok ()

    method create_materials =
      super#create_materials ;
      self#add_material "vnc_vertex" "vnc_vertex" [|"V"; "M"|] ;
      Ok ()

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
      let objs = [(axes :> ogl_obj); new ogl_obj_data; ] in
      let widget = new ogl_widget_plot app.Ogl_app.Builder.stylesheet name_values in
      widget#set_objs objs;
      widget#name_value_args name_values;
      Ogl_app.Builder.add_child app (widget :> ogl_widget)
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
  match (Ogl_app.Builder.create_app_from_xml app_xml stylesheet xml_additions (fun displays -> (new ogl_app_plot stylesheet displays))) with
    None -> 
    (
      Printf.printf "Failed to create app\n"; exit 1
    )
  | Some app ->
     (

       match (Sdl_ogl_gui.run_app app) with
         Ok () -> exit 0
       | Error msg -> Printf.printf "%s\n" msg; exit 1
     )

let () = main ()

