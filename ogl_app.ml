(*a Includes *)
open Stylesheet
open Sax
open Ogl_program
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
    ignore (Stylesheet.build stylesheet (List.map (fun od -> od#get_stylable) ogl_displays));
    Stylesheet.apply_stylesheet stylesheet;
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

(*a Builder module
 *
 * Permits an application to be built from a XML file
 *
 *)
module Builder = struct

  (*t stack_entry_element - element of the widget stack used in building the app from XML
   *)
  type stack_entry_element = ((string * string) list * (ogl_widget option))

  (*t stack_entry - list of widget/options that is a stack entry when building from XML
   *)
  type stack_entry         = stack_entry_element list

  (*t t - Application builder state
   *)
  type t = 
    {
      app_creator : (ogl_widget_display list) -> ogl_app;
      stylesheet : Stylesheet.t;
      xml_additions : (string * (t -> string -> (string * string) list -> unit) ) list;
      mutable widget_stack : (string * Sax.attribute list) list;
      mutable children_stack : stack_entry list;
      mutable current_children : stack_entry;
      mutable displays : (ogl_widget_display list);
      mutable app : ogl_app option;
    }

  (*f create - create an application builder state
   *)
  let create app_creator stylesheet xml_additions= {app_creator; stylesheet; xml_additions; widget_stack=[]; children_stack=[]; current_children=[]; displays=[]; app=None}

  (*f list_head_tail - return list head and tail as a tuple
   *)
  let list_head_tail l = 
    match l with
      [] -> (Printf.printf "List empty\n%!"; raise Not_found)
    | hd::tail -> (hd, tail)

  (*f push_widget - push a widget+options on to the current stack_entry, and make it the current parent for children
   *)
  let push_widget t name atts=
    t.widget_stack <- (name, atts)::t.widget_stack;
    t.children_stack <- t.current_children::t.children_stack;
    t.current_children <- [];
    ()

  (*f add_child - add a child to the list of children of the current widget
   *)
  let add_child t widget =
    t.current_children <- ([], Some widget)::t.current_children;
    ()

  (*f add_grid_element - add options and a widget as a child of the current widget
   *)
  let add_grid_element t name_values child =
    t.current_children <- (name_values, Some child)::t.current_children;
    ()

  (*f add_grid_span - add options as a child of the current widget
   *)
  let add_grid_span t name_values =
    t.current_children <- (name_values, None)::t.current_children;
    ()

  (*f iter_children - apply a function to every widget in a child list
   *)
  let iter_children f children =
    List.iter (fun nvs_w ->
                let (_,opt_w)=nvs_w in
                if option_is_some opt_w then
                  let w = option_get opt_w in
                  f w
              )
              children

  (*f children_nth_widget - return the nth child widget option in  a children list
   *)
  let children_nth_widget n children =
    if ((List.length children)<=n) then
      None
    else
      let (_,opt_w) = List.nth children n in
      opt_w

  (*f pop_widget - get options and children list from top of children_stack and widget_stack
   *)
  let pop_widget t =
    let children = t.current_children in
    let (parents_children,rest_children) = list_head_tail t.children_stack in
    let ((name,atts),rest_widgets)       = list_head_tail t.widget_stack in
    t.children_stack <- rest_children;
    t.widget_stack <- rest_widgets;
    t.current_children <- parents_children;
    (name, atts, children)

  (*f make_app - Call the app creator on the displays already created, to create the app
   *)
  let make_app t children =
    t.app <- Some (t.app_creator t.displays);
    ()

  (*f add_display - Add a display to the end of the display list
   *)
  let add_display t display =
    display#set_depth 0;
    t.displays <- t.displays@[display];
    ()

  (*f get_opt_app - get the app option
   *)
  let get_opt_app t = 
    t.app

  (*f start_element - Invoke at XML open tag; push a widget on to the widget stack
   *)
  let start_element app ~uri ~localName ~qName ~atts =
    push_widget app localName atts;
    ()

  (*f end_element - Invoke at XML close tag; finalize the widget at the top of the widget stack
   *)
  let end_element app ~uri ~localName ~qName =
    let (st_name, atts, children) = pop_widget app in
    let name_values = Sax.Attributes.nameValues atts in
    if (List.mem_assoc st_name app.xml_additions) then (
      (List.assoc st_name app.xml_additions) app st_name name_values
    )
    else
      (
        match st_name with
          "label" -> 
          (
            let text     = (Sax.Attributes.getValue ~default:""   atts "text") in
            let widget = new ogl_widget_text app.stylesheet name_values in
            widget#name_value_args name_values;
            ignore (widget#set_text text);
            add_child app (widget :> ogl_widget)
          )
        | "window" -> 
           (
             let display = new ogl_widget_display app.stylesheet name_values None in
             display#name_value_args name_values;
             iter_children (fun w -> display#add_child w) children;
             add_display app display
           )
        | "box" -> 
           (
             let widget = new ogl_widget_box app.stylesheet name_values in
             widget#name_value_args name_values;
             iter_children (fun w -> widget#add_child w) children;
             add_child app widget
           )
        | "grid_element" -> 
           (
             let opt_w = children_nth_widget 0 children in
             if (option_is_some opt_w) then
               add_grid_element app name_values (option_get opt_w)
           )
        | "grid_span" -> 
           (
             let grid_span = (name_values) in
             add_grid_span app grid_span
           )
        | "grid" -> 
           (
             let widget = new ogl_widget_grid app.stylesheet name_values in
             widget#name_value_args name_values;
             let element_or_span_data nvs_ow =
               let (nvs,opt_w) = nvs_ow in
               if option_is_some opt_w then
                 let w = option_get opt_w in
                 (widget#set_element nvs w)
               else
                 (widget#set_span_data nvs)
             in
             List.iter element_or_span_data children;
             widget#grid_build;
             add_child app (widget :> ogl_widget)
           )
        | "app" -> 
           (
             make_app app children
           )
        | _ -> ()
      )

  (*f create_app_from_xml - Create an app from an XML *)
  let create_app_from_xml app_xml stylesheet xml_additions app_creator = 
    let app = create app_creator stylesheet xml_additions in
    let ogl_sax_content_handler =
      Content_handler.create
        ~start_element_callback:start_element
        ~end_element_callback:end_element
        ~callback_state:app
        ()
    in
    let ogl_sax_parser = Sax.XMLReader.create ~content_handler:ogl_sax_content_handler () in
    match Sax.XMLReader.parse_string ogl_sax_parser app_xml with
      Error msg -> ( Printf.printf "%s\n" msg; None )
    | Ok ()     -> ( get_opt_app app)

  (*f create_app_from_xml_file - Create an app from an XML file *)
  let create_app_from_xml_file app_xml_file stylesheet xml_additions app_creator = 
    let read_file f = 
      let rec read acc = 
      try
        let l = input_line f in
        read (l::acc)
      with _ -> acc
      in
      String.concat "\n" (List.rev (read []))
    in
    let app_xml = (read_file app_xml_file) in
    Printf.printf "%s\n" app_xml ;
    create_app_from_xml app_xml stylesheet xml_additions app_creator

    (*f All done *)
end

