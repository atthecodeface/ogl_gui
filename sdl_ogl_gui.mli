type 'a sdl_result = ('a, [ `Msg of string ]) result
type t_win_handle = Ogl_types.t_window_handle
val ( >>>= ) :
  'a sdl_result -> ('a -> ('b, string) result) -> ('b, string) result
module Sdl_ogl_window :
  sig
    type t = { win : Tsdl.Sdl.window; ctx : Tsdl.Sdl.gl_context; }
    val create :
      ?width:int -> ?height:int -> ?title:string -> 'a -> t Utils.ogl_result
    val destroy : t -> (unit, 'a) result
    val reshape :
      t -> 'a -> 'b -> < reshape : 'c -> 'a -> 'b -> 'd; .. > -> 'c -> 'd
    val draw : t -> < draw : 'a -> 'b; .. > -> 'a -> unit
    val is_window : Tsdl.Sdl.window -> t -> bool
  end
module Sdl_ogl_app :
  sig
    type t = {
      app : Ogl_types.t_ogl_app;
      win_ctxs : (t_win_handle * Sdl_ogl_window.t) list ref;
      start_time : Tsdl.Sdl.uint32;
    }
    val create_window :
      ?width:int ->
      ?height:int ->
      ?title:string -> t -> t_win_handle -> t_win_handle Utils.ogl_result
    val init : Ogl_types.t_ogl_app -> t Utils.ogl_result
    val destroy : t -> (unit, 'a) result
    val wh_wc_of_wh :
      t -> t_win_handle -> (t_win_handle * Sdl_ogl_window.t) option
    val wh_wc_of_win :
      t -> Tsdl.Sdl.window option -> (t_win_handle * Sdl_ogl_window.t) option
    val wh_of_wid : t -> int -> t_win_handle option
    val wh_of_win : t -> Tsdl.Sdl.window option -> t_win_handle option
    val get_mouse_wxyb :
      t -> unit -> t_win_handle option * int * int * Tsdl.Sdl.uint32
    val get_key_modifiers : int -> int
    val key_event :
      t ->
      Tsdl.Sdl.event ->
      t_win_handle option * Tsdl.Sdl.keycode * Tsdl.Sdl.keymod
    val handle_key : t -> Ogl_types.t_key_action -> Tsdl.Sdl.event -> unit
    val int_of_int32 : int32 -> int
    val handle_mouse :
      t ->
      Ogl_types.t_mouse_action ->
      int -> int32 -> int -> int -> int array -> unit
    val reshape : t -> Tsdl.Sdl.window option -> int -> int -> unit
    val draw : t -> Tsdl.Sdl.window option -> unit
    val redraw : t -> t_win_handle -> unit
    val handle_window_event : t -> Tsdl.Sdl.event -> unit
    val event_loop : t -> (unit, 'a) result
  end
val run_app :
  ?width:int ->
  ?height:int ->
  ?title:string -> 
  ?ogl_root_dir:string ->
  Ogl_app.ogl_app -> (unit, string) Result.result
