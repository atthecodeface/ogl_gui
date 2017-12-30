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

open Atcflib
open Tgl4
open Ogl
open Sdl_ogl
open Stylesheet
open Dem
open Glprogram

(*f Load font to start with *)
open Font
let outline_font = Font.Outline.load_json "cabin-bold"

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

class ogl_obj_dem dem =
    object (self)
      inherit Ogl.ogl_obj as super
      val mutable num_pts = -1;
      method create_geometry ~offset  =
        let nrows = Dem.Header.nrows dem.Dem.hdr in
        let ncols = Dem.Header.ncols dem.Dem.hdr in
        let (opt_min,opt_max) = Dem.find_min_max dem in
        let min = option_get opt_min in
        let max = option_get opt_max in
        let scale = (2.0 /. (max -. min)) /. 20. in
        let base = (max -. min)/.2.0 in
        Printf.printf "Data has %d cols %d rows\n\n" ncols nrows;
        num_pts <- nrows * ncols ;
        let ba_float_array len = Bigarray.(Array1.create float32 c_layout len) in
        let axis_vertices = ba_float_array (num_pts * 3) in
        let do_pt x y d acc =
          let pt = ((y*ncols)+x)*3 in
          axis_vertices.{pt+0} <- ((float x) /. (float ncols)) -. 0.5;
          axis_vertices.{pt+1} <- (d-.base) *. scale;
          axis_vertices.{pt+2} <- ((float y) /. (float nrows)) -. 0.5;
          acc
        in
        Dem.fold_xy dem None do_pt;
        Printf.printf "Data %f,%f,%f\n\n" axis_vertices.{0} axis_vertices.{1} axis_vertices.{2};
        let size = Gl.bigarray_byte_size axis_vertices in
        vao_glid    <- gl_int_val (Gl.gen_vertex_arrays 1);
        Gl.bind_vertex_array vao_glid;
        let gl_id      = gl_int_val (Gl.gen_buffers 1) in
        Gl.bind_buffer Gl.array_buffer gl_id;
        Gl.buffer_data Gl.array_buffer size (Some axis_vertices) Gl.static_draw;
        Gl.enable_vertex_attrib_array 0;
        Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
        vertex_data_glids <- [gl_id];
        Ok () 
      method draw =
        (*Gl.enable_vertex_array ();*)
        Gl.bind_vertex_array vao_glid;
        Gl.draw_arrays Gl.points 0 num_pts;
        Gl.bind_vertex_array 0
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
    (*f create *)
    method create app =
      super#create app ;
      let gl_program_desc = Gl_program.make_desc "shaders/point_viewer_vertex.glsl" "shaders/fragment.glsl" [] ["M"; "V"; "G"; "P";] in
      app#add_program "banana" gl_program_desc >>= fun _ ->
         let program = app#get_program "banana" in
         let m_uid      = Gl_program.get_uniform_id "M"   program in
         let v_uid      = Gl_program.get_uniform_id "V"   program in
         let g_uid      = Gl_program.get_uniform_id "G"   program in
         let p_uid      = Gl_program.get_uniform_id "P"   program in
         opt_program <- Some (program, m_uid, v_uid, g_uid, p_uid);
         self#create_geometry;
         idler_handle <- app#add_idler self#idle ;
         Ok ()
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
      (*let dem = Dem.read_dat "dem_544000.258000.1.dat" in*)
      (*let dem = Dem.read_dat "dem_549000.259000.2.dat" in*)
      let dem = Dem.read_dat "dem_364000.586000.1.dat" in
      let objs = [axes; new ogl_obj_dem dem; ] in
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

