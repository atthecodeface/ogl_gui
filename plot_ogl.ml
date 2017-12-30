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

open Sdl_ogl_gui
open Atcflib
open Tgl4

(*f Load font to start with *)
open Font
let outline_font = Font.Outline.load_json "cabin-bold"

(*f read_data_file - read a whole file in as a string *)
let read_data_file filename fmt fn = 
  let f = open_in filename in
  let rec read acc n =
    try
      let l = input_line f in
      if (l.[0]='#') then
        read acc n
      else if (n<0) then
        read acc (n+1)
      else
        read ((Scanf.sscanf l fmt fn)::acc) 0
    with End_of_file -> close_in f ; acc
  in
  List.rev (read [] 0)

let blah x y = 
    let t = (Int64.to_float x)/.1000.0/.(7. *. 24. *. 3600.) in
    let tf = t -. (floor t) in
    let v = 1.0 -. (Int64.to_float y)/.100.0 in
    (tf,v)

let data = read_data_file "a.dat" " %Ld %_Ld %Ld %_Ld %_Ld %_Ld" blah
;;

Printf.printf "Read the data (%d points)\n" (List.length data)

let sel_true          =  (fun e -> true)
let sel_cbox          =  Stylable.is_element_id "control"
let sel_type_button   =  Stylable.is_element_type "text_button"
let sel_cls_rotate    =  Stylable.has_element_class "rotate"
let sel_state_pressed   =  Stylable.is_element_state 0 3
let sel_state_hover   =  Stylable.is_element_state 0 2
let sel_state_enable  =  Stylable.is_element_state 0 1

let sel_button_rotate = fun e -> (sel_type_button e) && (sel_cls_rotate e)
let sel_hover_button  = fun e -> (sel_type_button e) && (sel_state_hover e)

let stylesheet = Ogl.create_stylesheet ()
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

