class ogl_app :
  Stylesheet.Stylesheet.t ->
  Ogl_types.t_ogl_display list -> Ogl_types.t_ogl_app
module Builder :
  sig
    type stack_entry_element =
        (string * string) list * Ogl_widget.ogl_widget option
    type stack_entry = stack_entry_element list
    type t = {
      app_creator : Ogl_widget.ogl_widget_display list -> ogl_app;
      stylesheet : Stylesheet.Stylesheet.t;
      xml_additions :
        (string * (t -> string -> (string * string) list -> unit)) list;
      mutable widget_stack : (string * Sax.attribute list) list;
      mutable children_stack : stack_entry list;
      mutable current_children : stack_entry;
      mutable displays : Ogl_widget.ogl_widget_display list;
      mutable app : ogl_app option;
    }
    val create :
      (Ogl_widget.ogl_widget_display list -> ogl_app) ->
      Stylesheet.Stylesheet.t ->
      (string * (t -> string -> (string * string) list -> unit)) list -> t
    val list_head_tail : 'a list -> 'a * 'a list
    val push_widget : t -> string -> Sax.attribute list -> unit
    val add_child : t -> Ogl_widget.ogl_widget -> unit
    val add_grid_element :
      t -> (string * string) list -> Ogl_widget.ogl_widget -> unit
    val add_grid_span : t -> (string * string) list -> unit
    val iter_children : ('a -> unit) -> ('b * 'a option) list -> unit
    val children_nth_widget : int -> ('a * 'b option) list -> 'b option
    val pop_widget : t -> string * Sax.attribute list * stack_entry
    val make_app : t -> 'a -> unit
    val add_display : t -> Ogl_widget.ogl_widget_display -> unit
    val get_opt_app : t -> ogl_app option
    val start_element :
      t ->
      uri:'a ->
      localName:string -> qName:'b -> atts:Sax.attribute list -> unit
    val end_element : t -> uri:'a -> localName:'b -> qName:'c -> unit
    val create_app_from_xml :
      string ->
      Stylesheet.Stylesheet.t ->
      (string * (t -> string -> (string * string) list -> unit)) list ->
      (Ogl_widget.ogl_widget_display list -> ogl_app) -> ogl_app option
    val create_app_from_xml_file :
      in_channel ->
      Stylesheet.Stylesheet.t ->
      (string * (t -> string -> (string * string) list -> unit)) list ->
      (Ogl_widget.ogl_widget_display list -> ogl_app) -> ogl_app option
  end
