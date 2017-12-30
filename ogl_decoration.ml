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
open Utils
open Animatable
open Ogl_types
open Ogl_obj_standard
open Ogl_program
open Stylesheet

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
  method draw_border (app:t_ogl_app) (display:t_ogl_display) =
    if ((option_is_none gl_obj) || (border_triangles=0)) then ()
    else
     (let other_uids = display#set_material (app#get_material "widget_color") draw_transformation in
      if Animatable_linear_float.is_changing border_color then
         (Animatable_linear_float.time_step time border_color; time <- time + 1;);
(*    else
    (Animatable_linear_float.set_target 0 100 [|1.0-.bg_r;1.0-.bg_g;1.0-.bg_b|] border_color; time <- 0;)
);*)
      let bg_rgb = Animatable_linear_float.get_value border_color in
      let (bg_r, bg_g, bg_b) = (bg_rgb.(0), bg_rgb.(1), bg_rgb.(2)) in
       Gl.uniform3f other_uids.(0) bg_r bg_g bg_b;
       angle <- angle +. 0.01;
       (option_get gl_obj)#draw_subset 0 (bg_triangles*3);
       Gl.bind_vertex_array 0;
      ())

  (*f draw_background - draw the background (of size in last 'set_dims' centred on 0,0) using transform *)
  method draw_background (app:t_ogl_app) (display:t_ogl_display) =
    if ((option_is_none gl_obj) || (bg_triangles=0)) then ()
    else
      (let other_uids = display#set_material (app#get_material "widget_color") draw_transformation in
       let bg_rgb = face_color in
       let (bg_r, bg_g, bg_b) = (bg_rgb.(0), bg_rgb.(1), bg_rgb.(2)) in
       Gl.uniform3f other_uids.(0) bg_r bg_g bg_b;
       angle <- angle +. 0.01;
       (option_get gl_obj)#draw_subset 0 (bg_triangles*3);
       Gl.bind_vertex_array 0;
      ())

end

