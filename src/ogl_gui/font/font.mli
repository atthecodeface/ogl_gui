module Glyph :
  sig
    type t_metrics = { advance_width : float; }
    type t = {
      name : string;
      unichr : Uchar.t;
      triangles : (int * int * int) list;
      points : (float * float) list;
      metrics : t_metrics;
    }
  end
module Metrics :
  sig
    type t = {
      fontname : string;
      line_gap : float;
      ascent : float;
      descent : float;
      line_spacing : float;
      space_width : float;
    }
  end
module Outline :
  sig
    type t = { metrics : Metrics.t; glyphs : (string * Glyph.t) list; }
    val load_json : string -> t option
    val get_glyph :
      ?xsc:float ->
      ?ysc:float ->
      ?xofs:float ->
      ?yofs:float ->
      t ->
      string ->
      (float * (float * float) list * (int * int * int) list) option
    val get_glyph_or_blank :
      ?xsc:float ->
      ?ysc:float ->
      ?xofs:float ->
      ?yofs:float ->
      t -> string -> float * (float * float) list * (int * int * int) list
    val get_line_spacing : t -> float
  end
