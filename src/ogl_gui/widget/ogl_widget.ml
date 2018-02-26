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
open Result
open Bigarray
open Font
open Utils
open Animatable
open Ogl_types
open Layout
open Obj
open Ogl_view
open Stylesheet
module Styleable = Stylesheet.Styleable

let font_default = option_get (Font.Outline.load_json "cabin-bold")

(*a Stylesheet things *)
let create_stylesheet _ = 
  let stylesheet = Stylesheet.create () in
  Stylesheet.add_style_defaults stylesheet [("border",  Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("padding", Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("margin",  Styleable_value.Sv_float_6 [|0.;0.;0.;0.;0.;0.;|], false);
                                            ("dims",    Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("offset",  Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("align",   Styleable_value.Sv_float_3 [|0.;0.;0.;|], false);
                                            ("faces",   Styleable_value.Sv_int_6 [|0;0;0;0;0;0;|], false);
                                            ("fill",    Styleable_value.Sv_int_3 [|0;0;0;|], false);
                                            ("width",   Styleable_value.Sv_float 0., false);
                                            ("height",   Styleable_value.Sv_float 0., false);
                                            ("face_color",   Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("border_color", Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("bg_color",     Styleable_value.Sv_rgb [|0.;0.;0.;|], true); (* inherit *)
                                            ("font_size",    Styleable_value.Sv_float 1., true); (* inherit *)
                                            ("font_height",    Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_thickness", Styleable_value.Sv_float 0., true); (* inherit *)
                                            ("font_color",    Styleable_value.Sv_rgb [|1.;1.;1.;|], true); (* inherit *)
                                           ];
    stylesheet

let widget_decorator_styles = [ ("padding", Styleable_value.St_float_6);
                         ("margin",  Styleable_value.St_float_6);
                         ("border",  Styleable_value.St_float_6);
                         ("faces",   Styleable_value.St_int_6);
                         ("border_color", Styleable_value.St_rgb );
                         ("face_color", Styleable_value.St_rgb );
             ]
let widget_base_styles = widget_decorator_styles @ [ ("dims", Styleable_value.St_float_3);
(* dims INCLUDING margin/border/padding - CSS box model *)
                      ("fill", Styleable_value.St_int_3 );
                      ("align", Styleable_value.St_float_3 );
                      ("offset", Styleable_value.St_float_3 );
             ]
let styleable_act_level =  ("activity_level" , [ ("disable",0); ("enable",1); ("hover",2); ("pressed",3);])
let widget_grid_styles = widget_base_styles
let widget_text_styles = [ ("font_color", Styleable_value.St_rgb);
                           ("font_size", Styleable_value.St_float);
                           ("font_height", Styleable_value.St_float);
                           ("font_thickness", Styleable_value.St_float);
    ] @ widget_base_styles
let widget_display_styles = [ ("width", Styleable_value.St_float);
                               ("height", Styleable_value.St_float);
    ] @ widget_base_styles
let styleable_widget_box_desc     = Stylesheet.create_desc [styleable_act_level] widget_base_styles
let styleable_widget_grid_desc    = Stylesheet.create_desc [styleable_act_level] widget_grid_styles
let styleable_widget_text_desc    = Stylesheet.create_desc [styleable_act_level] widget_text_styles
let styleable_widget_viewer_desc  = Stylesheet.create_desc [styleable_act_level] widget_base_styles
let styleable_widget_display_desc = Stylesheet.create_desc [styleable_act_level] widget_display_styles

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
    val decoration   = new Ogl_decoration.ogl_decoration;
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

(*c ogl_widget_box  *)
class ogl_widget_box stylesheet name_values =
  object (self)
    inherit ogl_widget stylesheet styleable_widget_box_desc "box" name_values  as super
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

    inherit ogl_widget stylesheet styleable_widget_grid_desc "grid" name_values  as super

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
    inherit ogl_widget stylesheet styleable_widget_text_desc "text" name_values   as super
    val mutable text = "not banana";
    val mutable font_size_ref       = Styleable_value.svr_zero;
    val mutable font_height_ref     = Styleable_value.svr_zero;
    val mutable font_thickness_ref  = Styleable_value.svr_zero;
    val mutable font_color_ref      = Styleable_value.svr_zero;
    val mutable font_size           = 0.;
    val mutable font_height         = 0.;
    val mutable font_thickness      = 0.; (* Currently not supported by ogl_obj_text *)
    val mutable font_color  = Animatable_linear_float.create [|0.;0.;0.;|]
    val mutable font = font_default;
    val mutable text_dims = [|0.;0.;0.|];
    val mutable obj = None;

  method create app =
    super#create app >>=
      fun _ ->
      (
        font_color_ref     <- Stylesheet.se_get_value_ref self#get_styleable "font_color" ;
        font_size_ref      <- Stylesheet.se_get_value_ref self#get_styleable "font_size" ;
        font_height_ref    <- Stylesheet.se_get_value_ref self#get_styleable "font_height" ;
        font_thickness_ref <- Stylesheet.se_get_value_ref self#get_styleable "font_thickness" ;
        font_size          <- Styleable_value.ref_value_as_float font_size_ref;
        font_height        <- Styleable_value.ref_value_as_float font_height_ref;
        font_thickness     <- Styleable_value.ref_value_as_float font_thickness_ref;
        Animatable_linear_float.set_value font_color (Styleable_value.ref_value_as_floats font_color_ref);
        self#set_text text
      )

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
        let text_obj = new Obj.ogl_obj_text ~size:fsize ~height:fheight font text in
        let (x0,x1,y0,y1,z0,z1) = text_obj#get_bbox in
        text_dims <- [|x1 -. x0; y1 -. y0; z1 -. z0|];
        text_obj#create_geometry ~offset:(((x0 +. x1) *. (-. 0.5)), ((y0 +. y1) *. (-. 0.5)), 0.) >>=
          fun _ ->
          ( obj <- Some text_obj; Ok () )
    end
  method get_content_desired_dims = 
    text_dims
  method draw_content view_set transformation =
    if (option_is_none obj) then ()
    else
      (let other_uids = Ogl_view.set view_set (Ogl_view.get_material view_set "widget_color") transformation in
      let rgb = Animatable_linear_float.get_value font_color in
      Gl.uniform3f other_uids.(0) rgb.(0) rgb.(1) rgb.(2);
      (option_get obj)#draw view_set other_uids;
      Gl.bind_vertex_array 0;
      ())

    (*f style_change *)
    method style_change sid_svs =
      if (self#can_create) then (
        Animatable_linear_float.set_value font_color (Styleable_value.Styleable_value_ref.get_value_as_floats font_color_ref);
        super#style_change sid_svs;
      );
      ()

    (*f mouse - handle a mouse action along the action vector *)
    val mutable mouse_state = 0;
    method mouse_action action mouse vector options =
      let (cr,max_d) = vector in
      let opt_k = self#intersect_ray cr in
      match mouse_state with
        0 -> (if ((action<>Mouse_action_motion) || (option_is_none opt_k)) then McbNone else
                (mouse_state <- 1;Styleable.set_element_state 0 2 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
                 )
       | 1 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Styleable.set_element_state 0 1 self#get_styleable;Stylesheet.apply stylesheet;McbNone)
            else if (action=Mouse_action_down) then
             (mouse_state <- 2;Styleable.set_element_state 0 3 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
            else (McbSome (self#mouse_action))
           )
       | 2 -> (if (option_is_none opt_k) then
              (mouse_state <- 0;Styleable.set_element_state 0 1 self#get_styleable;Stylesheet.apply stylesheet;McbNone)
            else if (action=Mouse_action_up) then
              (
                (self#get_app)#button_pressed (self:>t_ogl_widget);
                mouse_state <- 1;Styleable.set_element_state 0 2 self#get_styleable;Stylesheet.apply stylesheet;McbSome (self#mouse_action))
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
module Ordint = struct type t=int let compare a b = Pervasives.compare a b end
module Intset=Set.Make(Ordint)
class ogl_widget_viewer stylesheet name_values  = 
  object (self)
    inherit ogl_widget stylesheet styleable_widget_viewer_desc "viewer" name_values  as super
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

(*c ogl_widget_display - Display which contains a single widget
  This owns an OpenGL context and an openGL render buffer (which might
  be an OS window)

  A material is a program, a P, a G, and another 'other parameters' uniform
  P is always set to 'projection'
  G is a per-widget 'GUI' projection - this may rotate, translate, and so on, in to a [-1;+1]  cube
  Other parameters would include M and V for an 3D-world, a color for a material, and so on
  set_material <program> <G> <other> should
  check if program is current, and if not select it and set the P uniform
  It should set the G uniform and other uniform

  The 'draw_content' for a widget should set its material

  The decoration 'draw_border' and 'draw_background' should set their material
  *)
let screen_ppmm   = 5. (* 5 pixels per mm *)
let screen_2ppmm  = 2. *. screen_ppmm
let screen_depth_mm = 100. (* 10cm deep in, 10 cm out *)

class ogl_widget_display stylesheet name_values (toplevel_init:t_ogl_widget option): t_ogl_display = 
object (self)
  inherit ogl_widget stylesheet styleable_widget_display_desc "display" name_values  as super
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
  val mutable current_material : Glprogram.Material.t option = None
  initializer
    if option_is_some toplevel_init then
     super#add_child (option_get toplevel_init) ;
    ()

  method get_width_height =
    let flt_width  = Styleable_value.ref_value_as_float (Stylesheet.se_get_value_ref self#get_styleable "width") in
    let flt_height = Styleable_value.ref_value_as_float (Stylesheet.se_get_value_ref self#get_styleable "height") in
    (max (int_of_float flt_width) 80, max (int_of_float flt_height) 80)

  method create app_init =
    ignore (Quaternion.assign_of_rotation (Vector.make3 0.1 0.9 0.1) (cos 0.003) (sin 0.003) playq2) ;
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

  method set_material opt_material widget_transformation =
    let transformation = ba_of_matrix4 widget_transformation in
    if ((Option.is_none current_material) || (Option.is_none opt_material) || ((Option.get opt_material) != (Option.get current_material))) then (
      current_material <- opt_material;
      Glprogram.Material.set_projection (option_get opt_material) (ba_of_matrix4 projection) transformation
    ) else (
      Glprogram.Material.set_transformation (option_get opt_material) transformation
    )
 
  method display_draw = 
    Gl.enable Gl.depth_test;
    Gl.clear_color 0.5 0.5 0.5 1.;
    Gl.clear (Gl.color_buffer_bit lor Gl.depth_buffer_bit);
    ignore (Matrix.identity play1);
    ignore (Quaternion.premultiply playq2 playq1);
    ignore (Matrix.assign_from_q playq1 play1);
    ignore (Matrix.assign_m_m projection play1 play2); (* can set projection to be play2 *)
    current_material <- None;
    let view_set = Ogl_view.create (option_get app) (self :> t_ogl_display) (*display#set_material*) in
    self#draw view_set

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

