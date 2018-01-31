open Utils
open Atcflib
open Ogl_program
open Stylesheet

(*a Class types for OpenGL widget, app and display, which are interrelated *)
(*t Enmerations *)
type t_mouse_action = 
  Mouse_action_down
| Mouse_action_up
| Mouse_action_wheel
| Mouse_action_motion

type t_key_action =
  Key_action_press
| Key_action_release

(*t t_window_handle - owned by ogl_app, given to OS-based infrastructure to permit it to control and report events to windows *)
type t_window_handle = int

(*t t_create_window_fn - a function of this type is provided by the OS-based infrastructure to allow the app to create a real OS window (and OpenGL context) *)
type t_create_window_fn = width:int -> height:int -> title:string -> t_window_handle -> t_window_handle ogl_result

(*c t_ogl_widget
  Units used by widgets are mm.

  The screen viewport is mapped from pixels to mm by the toplevel
  display.  Potentially a projection can scale the widgets, but that
  does not effect the widget itself.

  A widget has a desired size.

  This is derived from the larger of the widget's own content or its
  children laid out to their desired size.
  Added to this is the widget's decoration size (margin, border, padding)

  When a widget has been laid out it is given a view matrix (which is
  effectively how the widget is viewed), dimensions (in mm) for three
  dimensions, and an offset from (0,0,0) that the widget should be
  placed at. The widget has decoration, of course, so the content of
  the widget has potentially to be offset by a different amount if the
  decoration is lop-sided. The widget has children too; they are laid
  out within the dimensions, and they can then have their layout told
  to them with a new view matrix (that for this widget's content),
  dimensions (of the child widget as laid out), and offset (within the
  content of this widget).
  
 the placement layout at 2D is a, (x,y,w,h) for each widget, by name. The placement is attached to a 2D widget.
 x, y, w, h may be animatable or constant floats.
 the placement layout is informed of a placement of a widget using 'place <widget name> <x> <y> <w> <h>'
 if any of x,y,w,h change then the placer will ask its widget for a redraw, and the widget will ask its parent, and so on.

 the animatables must all be linked to an animator which is attached to the toplevel widget which can have an idler if any animatable is currently animating.

  a widget should be able to have the states Disabled, Highlighted, Normal

  as a result of the state the widget should have desired foreground color, background color, and border colors; complete with transparencies.
  The colors should therefore be animatable; if the state changes from one to another, then the colors change over a specified time.

  and animatable is then a value, a target value, and a time to get to that value, and an interpolation method.
  potentially text could have a rotation, which would be expressed as a quaternion, which could then be animatable

  a widget should be able to have a desired width / height / depth, or desired minimums for these. If not fixed then they should depend on the content (there own and children)

  a button widget should be able to have a checkbutton, radio button, radio button group, text, and additionaly children
  a checkbutton mark is a cuboid with a check object or cross object to the left, right, above, below, front or back of the content
  a radiobutton mark is a cuboid with a sphere object to the left, right, above, below, front or back of the content
  text may be aligned left, right, top, bottom, front, back
  text may have a single font, font size, thickness
  possibly an 'image' may be permitted which is a texture applied to a flat surface
 *)

type t_action_vector = (Collider_ray.t * float) (* launch point + direction collider, action range *)
type t_mouse_callback_result = McbNone | McbSome of (t_mouse_action -> int -> t_action_vector -> int array -> t_mouse_callback_result)
type t_mouse_claimant = t_mouse_action -> int -> t_action_vector -> int array -> (t_mouse_callback_result)
type t_mouse_callback = t_mouse_action -> int -> t_action_vector -> int array -> (t_mouse_callback_result)
type t_mouse_result = (float * t_mouse_callback) option
type t_key_callback = t_key_action -> int -> int -> t_action_vector -> int option
type t_key_result = (float * t_key_callback) option
class type t_ogl_view_set = object
  method get_app  : t_ogl_app
  method get_display : t_ogl_display
  end
and t_ogl_widget = 
  object
    (* val children : ogl_widget list ;*)
    method name_value_args : (string * string) list -> unit
    method get_id : string
    method create_tree_styles : unit ogl_result (* Create tree of parent/children and stylables *)
    method create : t_ogl_app -> unit ogl_result (* After stylesheet, create objects, read styles etc - all set up *)
    method can_create : bool
    method get_app : t_ogl_app
    method destroy : unit
    method add_child : ?order:int -> t_ogl_widget -> unit
    method set_parent : t_ogl_widget -> unit
    method get_parent : t_ogl_widget option
    method get_children : t_ogl_widget list
    method get_depth : int
    method set_depth : int -> unit
    method get_styleable : Stylesheet.Styleable.t
    method get_content_desired_dims : float t_dims3
    method get_content_draw_dims    : float t_dims3
    method get_desired_dims : float t_dims3
    method style_change : Stylesheet.Styleable.t_style_change_callback
    method layout_get_desired_dims : float t_dims3  (* override with layout widget *)
    method layout           : float t_dims3 -> Matrix.t -> float t_dims3 -> unit
    method layout_content_with_dims  : float t_dims3 -> Matrix.t -> float t_dims3 -> unit (* override with layout widget *)
    method set_layout       : float t_dims3 -> Matrix.t -> float t_dims3 -> Matrix.t
    method draw             : t_ogl_view_set -> unit (* app so that OpenGL may be interacted with *)
    method draw_content     : t_ogl_view_set -> Matrix.t -> unit (* app so that OpenGL may be interacted with *)
    method intersect_ray    : Collider_ray.t -> float option
    method key              : t_key_action -> int -> int -> t_action_vector -> t_key_result
    method mouse            : t_mouse_action -> int -> t_action_vector -> int array -> t_mouse_result
    method request_redraw   : unit (* called by the widget or a child to request redraw *)
    method str : string
  end
(*c and t_ogl_display - a widget that is also an OS window and OpenGL context

  An ogl_display is an OS window, which is expected to contain one or more ogl_widgets

  The ogl_app despatches events to the requisite ogl_display based on a t_window_handle
  *)
and t_ogl_display = (* Widget that is a whole openGL context - or possibly fraction thereof *)
  object
    inherit t_ogl_widget
    method get_width_height : int*int
    method set_material : Ogl_program.Material.t option -> Atcflib.Matrix.t -> int array
    method display_reshape : int -> int -> unit (* Reshapes the toplevel window *)
    method display_draw    : unit (* Draw the context; draw self and children with correct projection *)
    method display_key     : t_key_action -> int -> int -> int -> int -> int option
    method display_mouse   : t_mouse_action -> int -> int -> int -> int array -> t_mouse_callback_result
    method display_mouse_claimant : t_mouse_action -> int -> int -> int -> int array -> t_mouse_callback -> t_mouse_callback_result
  end

(*c and t_ogl_app - one of these is required for the application
  The app will be called by SDL or GLUT or whatever when a key is pressed, the mouse is moved, and so on, over any OS window
  *)
and t_ogl_app =
  object
    method create_shaders : unit ogl_result
    method create_materials : (unit, string) Result.result
    method set_create_window : t_create_window_fn -> unit (* Invoked by SDL/GLUT to populate the create_window function *)
    method create_window : ?title:string -> t_ogl_display -> t_window_handle ogl_result
    method create      : string -> unit ogl_result
    method destroy     : unit ogl_result 
    method add_material : string -> string -> string array -> unit ogl_result
    method add_program : string -> Gl_program.desc -> unit ogl_result
    method add_display : string -> t_ogl_display   -> t_window_handle -> unit
    method get_program : string -> Gl_program.t
    method get_material : string -> Material.t
    method button_pressed : t_ogl_widget -> unit
    method key         : t_window_handle option -> t_key_action -> int -> int -> int -> int -> unit
    method mouse       : t_window_handle option -> t_mouse_action -> int -> int -> int -> int array -> unit
    method draw        : t_window_handle -> unit
    method reshape     : t_window_handle -> int -> int -> unit
    method request_redraw : t_ogl_display -> unit (* called by a toplevel widget to request redraw when need_redraw is set *)
    method need_redraw : t_window_handle list
    method idle        : int option
    method add_idler   : (unit -> int option) -> int
  end

class type t_ogl_obj = 
  object
    method create_geometry : offset:float * float * float -> unit Utils.ogl_result
    method delete_geometry : unit Utils.ogl_result
    method draw : unit
  end


