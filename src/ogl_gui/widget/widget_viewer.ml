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

(*a Classes *)
(*c ogl_widget_viewer, an OpenGL ogl_obj list viewer  *)
let vector_x_axis = Atcflib.Vector.make3 1. 0. 0.
let vector_y_axis = Atcflib.Vector.make3 0. 1. 0.
let vector_z_axis = Atcflib.Vector.make3 0. 0. 1.
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)
class ogl_widget_viewer stylesheet name_values  = 
  object (self)
    inherit Widget_base.ogl_widget stylesheet Styling.widget_viewer "viewer" name_values  as super
  val keys_down = ref Intset.empty
  val joystick_axes = Array.make 16 0;
  val direction = Quaternion.make_rijk 1.0 0. 0. 0.
  val scale   = ref 1.
  val center = Vector.make3 0. 0. 0.
  val z = Vector.make4 0. 0. 0. 0.
  val mutable idler_handle = -1
  val mutable draw_fn = let d a t = () in d
  val rotation = Matrix.make 4 4
  val translation = Matrix.make 4 4
  val view = Matrix.make 4 4
  val tmp = Matrix.make 4 4
  val tmp2 = Matrix.make 4 4
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
        ignore (Matrix.(set 1 1 ar_scale (set 0 0 ar_scale (identity tmp))));  (* Make -1/1 fit the width - but do not scale z *)
        ignore (Matrix.assign_m_m tmp view tmp2);  (* Make -1/1 fit the width - but do not scale z *)
        let other_uids = Ogl_view.set view_set (Some material) transformation in
        Gl.uniform_matrix4fv other_uids.(0) 1 true (ba_of_matrix4 tmp2); (* 0 -> V *)
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
        ignore (Matrix.row_vector rotation 2 z);
        ignore (Vector.add z center);
        ()

    (*f move_left *)
    method private move_left scale = 
        ignore (Matrix.assign_from_q direction rotation);
        ignore (Matrix.scale scale rotation);
        ignore (Matrix.row_vector rotation 0 z);
        ignore (Vector.add z center);
        ()

    (*f is_key_down *)
    method is_key_down k = Intset.mem (int_of_char k) !keys_down

    (*f joystick_axis_value *)
    method joystick_axis_value a =
      let v = joystick_axes.(a) in
      if (v < -1024) then v else if (v > 1024) then v else 0

    (*f idle *)
    method idle _ = 
      if self # is_key_down ',' then self#move_forward ((-0.01) /. !scale);
      if self # is_key_down 'l' then self#move_forward (0.01 /. !scale);
      if self # is_key_down 'q' then self#move_left ((-0.01) /. !scale);
      if self # is_key_down 'w' then self#move_left (0.01 /. !scale);
      if self # is_key_down '.' then self#pitch 0.005;
      if self # is_key_down ';' then self#pitch (-0.005);
      if self # is_key_down 'x' then self#yaw 0.005;
      if self # is_key_down 'z' then self#yaw (-0.005);
      if self # is_key_down 's' then self#roll 0.005;
      if self # is_key_down 'a' then self#roll (-0.005);
      if self # is_key_down '\'' then scale := !scale *. 1.05;
      if self # is_key_down '/' then  scale := !scale /. 1.05;
      let v = self # joystick_axis_value 2 in
      if (v!=0) then self # roll ((float v) /. 32768.0 /. 10.);
      let v = self # joystick_axis_value 3 in
      if (v!=0) then self # pitch ((float v) /. 32768.0 /. 10.);
      if Intset.mem 27 !keys_down then None else
        (self#request_redraw ; Some 10)

    (*f key - handle a keypress along the action vector *)
    method key action k meta vector =
      (match action with
         Key_action_press -> (keys_down := Intset.add k !keys_down)
       | Key_action_release -> (keys_down := Intset.remove k !keys_down)
      );
      None

    method joystick action which axis value options = 
      ( match action with
        | Joystick_action_axis -> (
          joystick_axes.(axis) <- value
        )
        | _ -> ()
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

