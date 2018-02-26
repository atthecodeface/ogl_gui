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
open Animatable
open Ogl_types
open Ogl_view
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a Styling *)
let styleable_widget_viewer_desc  = Stylesheet.create_desc [Widget_base.styleable_act_level] Widget_base.widget_base_styles

(*a Classes *)
(*c ogl_widget_viewer, an OpenGL ogl_obj list viewer  *)
let vector_x_axis = Atcflib.Vector.make3 1. 0. 0.
let vector_y_axis = Atcflib.Vector.make3 0. 1. 0.
let vector_z_axis = Atcflib.Vector.make3 0. 0. 1.
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)
class ogl_widget_viewer stylesheet name_values  = 
  object (self)
    inherit Widget_base.ogl_widget stylesheet styleable_widget_viewer_desc "viewer" name_values  as super
  val keys_down = ref Intset.empty
  val direction = Quaternion.make_rijk 1.0 0. 0. 0.
  val scale   = ref 1.
  val center = Vector.make3 0. 0. 0.
  val mutable idler_handle = -1
  val mutable draw_fn = let d a t = () in d
  val rotation = Matrix.make 4 4
  val translation = Matrix.make 4 4
  val view = Matrix.make 4 4
  val tmp = Matrix.make 4 4
  val q1 = Quaternion.make ()
  val q2 = Quaternion.make ()
  val q3 = Quaternion.make ()
  val mutable opt_material = None
  val mutable objs:Obj.ogl_obj list = []

    (*f create *)
    method create app =
      if (option_is_none opt_material) then (
          opt_material <- Some (app#get_material "p") ;
      );
      super#create app >>=
        fun _ ->
        (
          self#create_geometry;
          idler_handle <- app#add_idler self#idle ;
          Ok ()
        )

    method get_direction = direction
    method get_center    = center

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
    method draw_content view_set transformation =
      if (option_is_none opt_material) then () else
      begin    
        let material = (option_get opt_material) in
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.identity translation);
        ignore (Matrix.set 0 3 (-. (Vector.get center 0)) translation);
        ignore (Matrix.set 1 3 (-. (Vector.get center 1)) translation);
        ignore (Matrix.set 2 3 (-. (Vector.get center 2)) translation);
        ignore (Matrix.assign_m_m rotation translation view);
        let ar_scale = (min (super#get_content_draw_dims).(0) (super#get_content_draw_dims).(1)) *. 0.35 *. !scale in
        ignore (Matrix.scale ar_scale view);  (* Make -1/1 fit the width *)
        let other_uids = Ogl_view.set view_set (Some material) transformation in
        Gl.uniform_matrix4fv other_uids.(0) 1 true (ba_of_matrix4 view); (* 0 -> V *)
        Gl.uniform_matrix4fv other_uids.(1) 1 true identity4; (* 1 -> M *)
        List.iter (fun o -> o#draw view_set other_uids) objs;
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
        let z = (Matrix.row_vector rotation 2) in
        ignore (Vector.add z center);
        ()

    (*f move_left *)
    method private move_left scale = 
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.scale scale rotation);
        let z = (Matrix.row_vector rotation 0) in
        ignore (Vector.add z center);
        ()

    (*f idle *)
    method private idle _ = 
      if Intset.mem (int_of_char ',') !keys_down then self#move_forward ((-0.01) /. !scale);
      if Intset.mem (int_of_char 'l') !keys_down then self#move_forward (0.01 /. !scale);
      if Intset.mem (int_of_char 'q') !keys_down then self#move_left ((-0.01) /. !scale);
      if Intset.mem (int_of_char 'w') !keys_down then self#move_left (0.01 /. !scale);
      if Intset.mem (int_of_char '.') !keys_down then self#pitch 0.005;
      if Intset.mem (int_of_char ';') !keys_down then self#pitch (-0.005);
      if Intset.mem (int_of_char 'x') !keys_down then self#yaw 0.005;
      if Intset.mem (int_of_char 'z') !keys_down then self#yaw (-0.005);
      if Intset.mem (int_of_char 's') !keys_down then self#roll 0.005;
      if Intset.mem (int_of_char 'a') !keys_down then self#roll (-0.005);
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

