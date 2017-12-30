module Utf8 :
  sig
    type t = { c : int; s : string; }
    exception Bad_unicode
    val newline : t
    val _getc : char Stream.t -> string * int
    val getc : char Stream.t -> t
    val sc : t -> string * int
  end
module Attributes :
  sig
    type t = {
      qName : string;
      uri : string;
      localName : string;
      value : string;
      attr_type : string;
    }
    val create :
      qName:string ->
      uri:string ->
      localName:string -> value:string -> ?attr_type:string -> 'a -> t
    val nameValues : t list -> (string * string) list
    val getValue : default:string -> t list -> string -> string
  end
type attribute = Attributes.t
type 'a t_callback = 'a -> string -> unit
type 'a t_start_document_callback = 'a -> rootName:string -> unit
type 'a t_end_document_callback = 'a -> unit
type 'a t_start_element_callback =
    'a ->
    uri:string ->
    localName:string -> qName:string -> atts:attribute list -> unit
type 'a t_end_element_callback =
    'a -> uri:string -> localName:string -> qName:string -> unit
type 'a t_start_prefix_mapping_callback =
    'a -> prefix:string -> prefix:string -> unit
type 'a t_end_prefix_mapping_callback = 'a -> prefix:string -> unit
type 'a t_processing_instruction_callback =
    'a -> target:string -> data:string -> unit
type 'a t_skipped_entity_callback = 'a -> name:string -> unit
type 'a t_characters_callback =
    'a -> string -> start:int -> length:int -> unit
type 'a t_ignorable_whitespace_callback =
    'a -> string -> start:int -> length:int -> unit
module Content_handler :
  sig
    type 'a t = {
      start_document_callback : 'a t_start_document_callback option;
      end_document_callback : 'a t_end_document_callback option;
      start_element_callback : 'a t_start_element_callback option;
      end_element_callback : 'a t_end_element_callback option;
      start_prefix_mapping_callback :
        'a t_start_prefix_mapping_callback option;
      end_prefix_mapping_callback : 'a t_end_prefix_mapping_callback option;
      characters_callback : 'a t_characters_callback option;
      pi_callback : 'a t_processing_instruction_callback option;
      callback_state : 'a;
    }
    val create :
      ?start_document_callback:'a t_start_document_callback ->
      ?end_document_callback:'a t_end_document_callback ->
      ?start_element_callback:'a t_start_element_callback ->
      ?end_element_callback:'a t_end_element_callback ->
      ?start_prefix_mapping_callback:'a t_start_prefix_mapping_callback ->
      ?end_prefix_mapping_callback:'a t_end_prefix_mapping_callback ->
      ?characters_callback:'a t_characters_callback ->
      ?pi_callback:'a t_processing_instruction_callback ->
      callback_state:'a -> unit -> 'a t
    val invoke : 'a t -> 'b option -> ('b -> 'a -> 'c) -> unit
  end
module XMLReader :
  sig
    type 'a t = {
      mutable f : char Stream.t;
      mutable column : int;
      mutable line : int;
      mutable unget_buffer : Utf8.t list;
      mutable bad_unicode : bool;
      mutable unget_unicode : Utf8.t option;
      prolog_callback : 'a t_callback option;
      comment_callback : 'a t_callback option;
      content_handler : 'a Content_handler.t;
    }
    val create :
      ?prolog_callback:'a t_callback ->
      ?comment_callback:'a t_callback ->
      content_handler:'a Content_handler.t -> unit -> 'a t
    val is_whitespace : int -> bool
    val is_name_char : int -> bool
    val is_name_start_char : int -> bool
    val match_string : 'a t -> string -> bool
    val build_string : 'a t -> string -> (int -> bool) -> string
    val set_stream : 'a t -> char Stream.t -> unit
    val parse_content : 'a t -> (bool, string) result
    val parse_element : 'a t -> bool -> (bool, string) result
    val parse_string : 'a t -> string -> (unit, string) result
    val parse_stream : 'a t -> char Stream.t -> (unit, string) result
  end
