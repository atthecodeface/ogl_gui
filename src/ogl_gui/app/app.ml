(** Copyright (C) 2017-2018,  Gavin J Stark.  All rights reserved.
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
 * @file    app.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

(*a Includes *)
open Stylesheet
open Sax
open Glprogram
open Ogl_types
open Utils
open Ogl_widget

(*a OpenGL app virtual class and basic apps *)
(*c ogl_app *)
class ogl_app stylesheet ogl_displays : t_ogl_app =
object (self)
  (*b Properties *)
  val mutable program_list:(string*Gl_program.t) list = [];
  val mutable material_list:(string*Material.t) list = [];
  val mutable display_list:(string*t_ogl_display*t_window_handle) list = [];
  val mutable next_handle = 0;
  val mutable create_window_fn:t_create_window_fn = fun ~width ~height ~title handle -> Ok 0;
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
  method create_window ?title:(title="") display = 
    let handle = self#get_next_handle in
    let width,height = display#get_width_height in
    let wh = create_window_fn ~width:width ~height:height ~title:title handle in
    window_displays <- window_displays @ [(next_handle, display)];
    wh

  (*f display_of_handle - find ogl_display from a window handle to be returned to the OS side *)
  method private display_of_handle (handle:t_window_handle) =
    if (not (List.mem_assoc handle window_displays)) then None else Some (List.assoc handle window_displays)

  (*f add_display - add a name/ogl_display/window_handle to the app *)
  method add_display (name:string) display window_handle = 
    (display_list <- (name,display,window_handle)::display_list )

  (*f create_shaders - called during app creation *)
  method create_shaders =
    raise_any_error (self#add_program "p" (Gl_program.make_desc "vertex_obj_viewer.glsl" "fragment.glsl" [] ["M"; "V"; "G"; "P";] ))
     >>= fun _ ->
    raise_any_error (self#add_program "widget_color" (Gl_program.make_desc "widget_vertex.glsl" "widget_color_fragment.glsl" [] ["G"; "P"; "C"])) (* No M or V for a standard widget *)

  (*f create_materials - called during app creation *)
  method create_materials =
    self#add_material "widget_color" "widget_color" [|"C"|] ;
    self#add_material "p" "p" [|"V"; "M"|]

  (*f create - create the app *)
  method create ogl_root_dir = 
    Gl_program.reset_shader_path ();
    Gl_program.add_shader_path (sfmt "%s/shaders" ogl_root_dir);
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
    do_all_displays (fun od -> od#create_tree_styles)
    >>= fun _ ->
    ignore (Stylesheet.Stylesheet.build stylesheet (List.map (fun od -> od#get_styleable) ogl_displays));
    Stylesheet.Stylesheet.apply_stylesheet stylesheet;
    do_all_displays (fun od -> self#create_window ~title:"Display" od)
    >>= fun window_handles ->
    self#create_shaders
    >>= fun _ ->
    self#create_materials
    >>= fun _ ->
    do_all_displays (fun od -> od#create (self:>ogl_app))
    >>= fun _ ->
    List.iteri (fun i d -> self#add_display (sfmt "toplevel%d" i) d (List.nth window_handles i)) ogl_displays;
    Ok ()

  (*f add_program name program_desc - add an Gl_program.t OpenGL shader program (vertex + fragment) to the app *)
  method add_program (name:string) program_desc = 
    raise_any_error (Gl_program.make program_desc) >>=
    fun p -> (program_list <- (name,p)::program_list ; Ok () )

  (*f add_material name program other_uids - add a material (after program has been added) *)
  method add_material (name:string) (program:string) (other_uids:string array) = 
    let prog = self#get_program program in
    Material.create prog other_uids >>=
    fun m -> (material_list <- (name,m)::material_list ; Ok () )

  (*f get_program name - get the Gl_program.t of a named program that has been added *)
  method get_program name = List.assoc name program_list

  (*f get_material name - get the Material.t of a named material that has been added *)
  method get_material name = List.assoc name material_list

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

