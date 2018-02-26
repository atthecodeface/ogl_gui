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
open Batteries
open Atcflib
open Tgl4
open Bigarray
open Font
open Utils
open Animatable
open Ogl_types
open Ogl_view
open Styling
open Stylesheet
module Styleable = Stylesheet.Styleable

(*a OpenGL base widget class
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
class ogl_widget stylesheet styleable_desc widget_type name_values : t_ogl_widget = 
  
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
    val mutable opt_styleable = None
    val mutable is_button = false (* true => permit hover and pressed activity levels through mouse interaction *)
    val mutable bbox_draw_dims    = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val mutable bbox_draw_offset  = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val temp_matrix = Matrix.(identity (make 4 4)) (* Applied to (0,0,0) places widget center in the correct place *)
    val bbox_transformation   = Matrix.(identity (make 4 4)) (* Applied to (0,0,0) places widget center in the correct place *)
    val bbox_transformation_i = Matrix.(identity (make 4 4)) (* Inverse of bbox_transformation, used to convert ray in parent coords to local coords *)
    val mutable content_draw_dims = [|0.;0.;0.|]; (* Dimensions provided by layout *)
    val content_transformation = Matrix.(identity (make 4 4)) (* Accounts for alignment of content if it does not grow *)
    val internal_transformation = Matrix.(make 4 4)
    val decoration   = new Widget_decoration.ogl_decoration;
    val mutable id = "uncreated";
    val mutable opt_app = None;

    val mutable depth = 0;

    val mutable dims_ref       = Styleable_value.svr_zero;
    val mutable fill_ref       = Styleable_value.svr_zero;
    val mutable align_ref      = Styleable_value.svr_zero; (* -1.0 -> left, 0 center, 1.0 -> right *)

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
      let styleable_children = List.map (fun w -> w#get_styleable) children in
      let styleable = Stylesheet.se_create styleable_desc stylesheet widget_type name_values (self#style_change) styleable_children in
      opt_styleable <- Some styleable;
      id <- sfmt "%s.%s" (Styleable.get_type styleable) (Styleable.get_id styleable);
      List.iter (fun w -> Stylesheet.se_set_parent styleable w#get_styleable) children;
      Ok ()

    (*f create - create children, and set up layout and decorations, returning error if required *)
    method create app_init =
      opt_app <- Some app_init;
      let create_if_ok acc child =
        acc >>= fun () -> child#create app_init
      in
      List.fold_left create_if_ok (Ok ()) children >>= fun _ ->
      let styleable = self#get_styleable in
      dims_ref  <- Stylesheet.se_get_value_ref styleable "dims" ;
      fill_ref  <- Stylesheet.se_get_value_ref styleable "fill" ;
      align_ref <- Stylesheet.se_get_value_ref styleable "align" ;
      dims <- Styleable_value.ref_value_as_floats dims_ref;
      align <- Styleable_value.ref_value_as_floats align_ref;
      fill <- Styleable_value.ref_value_as_ints fill_ref;
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

    (*f get_styleable - get the Styleable that the widget is *)
    method get_styleable = (option_get opt_styleable)

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
    method draw view_set =
      (* draw decoration *)
      decoration#draw_border view_set;
      (* draw content *)
      self#draw_content view_set internal_transformation;
      (* draw children *)
      List.iter (fun c -> c#draw view_set) children;
      (* draw background *)
      decoration#draw_background view_set

    (*f get_content_desired_dims - get the xyz dimensions of the size of the content of the widget (override if a specific widget) *)
    method get_content_desired_dims = [|0.;0.;0.|]

    (*f get_content_draw_dims - get the xyz dimensions of the drawing size of the content of the widget, post-layout, use by subclasses *)
    method get_content_draw_dims = content_draw_dims

    (*f draw_content - draw the actual content of the widget (override if a specific widget) *)
    method draw_content view_set transformation = () (* Draw content *)

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

