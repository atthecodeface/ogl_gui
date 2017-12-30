(*
Some fonts may be licensed under the Open Font License (OFL)

Some fonts may be licensed under the Apache License

Cabin Bold - Open Font License:
Copyright (c) 2010 by Pablo Impallari. www.impallari.com
Copyright (c) 2010 by Igino Marini. www.ikern.com All rights reserved.

Noto-serif Bold - Open Font License:



 *)

(* Open Font license - this applies only to fonts distributed under
the OFL (NOT THIS SOFTWARE) - this is included here for reference
only.

This Font Software is licensed under the SIL Open Font License, Version 1.1.
This license is copied below, and is also available with a FAQ at:
http://scripts.sil.org/OFL


-----------------------------------------------------------
SIL OPEN FONT LICENSE Version 1.1 - 26 February 2007
-----------------------------------------------------------

PREAMBLE
The goals of the Open Font License (OFL) are to stimulate worldwide
development of collaborative font projects, to support the font creation
efforts of academic and linguistic communities, and to provide a free and
open framework in which fonts may be shared and improved in partnership
with others.

The OFL allows the licensed fonts to be used, studied, modified and
redistributed freely as long as they are not sold by themselves. The
fonts, including any derivative works, can be bundled, embedded, 
redistributed and/or sold with any software provided that any reserved
names are not used by derivative works. The fonts and derivatives,
however, cannot be released under any other type of license. The
requirement for fonts to remain under this license does not apply
to any document created using the fonts or their derivatives.

DEFINITIONS
"Font Software" refers to the set of files released by the Copyright
Holder(s) under this license and clearly marked as such. This may
include source files, build scripts and documentation.

"Reserved Font Name" refers to any names specified as such after the
copyright statement(s).

"Original Version" refers to the collection of Font Software components as
distributed by the Copyright Holder(s).

"Modified Version" refers to any derivative made by adding to, deleting,
or substituting -- in part or in whole -- any of the components of the
Original Version, by changing formats or by porting the Font Software to a
new environment.

"Author" refers to any designer, engineer, programmer, technical
writer or other person who contributed to the Font Software.

PERMISSION & CONDITIONS
Permission is hereby granted, free of charge, to any person obtaining
a copy of the Font Software, to use, study, copy, merge, embed, modify,
redistribute, and sell modified and unmodified copies of the Font
Software, subject to the following conditions:

1) Neither the Font Software nor any of its individual components,
in Original or Modified Versions, may be sold by itself.

2) Original or Modified Versions of the Font Software may be bundled,
redistributed and/or sold with any software, provided that each copy
contains the above copyright notice and this license. These can be
included either as stand-alone text files, human-readable headers or
in the appropriate machine-readable metadata fields within text or
binary files as long as those fields can be easily viewed by the user.

3) No Modified Version of the Font Software may use the Reserved Font
Name(s) unless explicit written permission is granted by the corresponding
Copyright Holder. This restriction only applies to the primary font name as
presented to the users.

4) The name(s) of the Copyright Holder(s) or the Author(s) of the Font
Software shall not be used to promote, endorse or advertise any
Modified Version, except to acknowledge the contribution(s) of the
Copyright Holder(s) and the Author(s) or with their explicit written
permission.

5) The Font Software, modified or unmodified, in part or in whole,
must be distributed entirely under this license, and must not be
distributed under any other license. The requirement for fonts to
remain under this license does not apply to any document created
using the Font Software.

TERMINATION
This license becomes null and void if any of the above conditions are
not met.

DISCLAIMER
THE FONT SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT
OF COPYRIGHT, PATENT, TRADEMARK, OR OTHER RIGHT. IN NO EVENT SHALL THE
COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
INCLUDING ANY GENERAL, SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL
DAMAGES, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF THE USE OR INABILITY TO USE THE FONT SOFTWARE OR FROM
OTHER DEALINGS IN THE FONT SOFTWARE.

 *)
open Uchar
open Yojson
open Yojson.Basic.Util
let assoc_assoc a k d  = match (member k a) with `Assoc v -> v | _ -> d
let assoc_string a k d = match (member k a) with `String v -> v | _ -> d
let assoc_float a k d  = match (member k a) with `Int    v -> (float v) | `Float v -> v | _ -> d
let assoc_int a k d    = match (member k a) with `Int    v -> v | _ -> d

(*a Glyph module *)
    (*
    An outline font glyph has the following attributes:

    name - name of the glyph, not used internally
    unichr - unicode character that the glyph represents
    metrics - dictionary of key->value, keys of
      advance_width     - amount to move X on after the glyph is rendered
      left_side_bearing - X displacement for rendering (e.g. letter y may have -ve value)
      xMin              - X bounding box minimum value
      xMax              - X bounding box maximum value
      yMin              - Y bounding box minimum value
      yMax              - Y bounding box maximum value
    glyph - contours making the outline
    mesh - mesh of triangles created from bezier curves
     *)
module Glyph = struct
  type t_metrics = {
      advance_width:float;
    }
    let metrics_from_json json = {advance_width=assoc_float json "advance_width" 0.;}
  type t = {
      name : string;
      unichr: Uchar.t;
      triangles : (int * int * int) list;
      points : (float * float) list;
      metrics : t_metrics;
    }
  let from_json json = 
    let pt_of_json json  = let l = convert_each to_number json in
        (List.nth l 0, List.nth l 1) in
    let tri_of_json json = let l = convert_each to_int json in
        (List.nth l 0, List.nth l 1, List.nth l 2) in
    let pts =  convert_each pt_of_json (index 0 json) in
    let tris = convert_each tri_of_json (index 1 json) in
    let metrics = metrics_from_json (index 2 json) in
    {name = ""; unichr=Uchar.of_int 32; triangles=tris; points=pts; metrics=metrics;}
end

(*a Font module *)
module Metrics = struct
  type t = {
      fontname : string;
      line_gap : float;
      ascent : float;
      descent : float;
      line_spacing : float;
      space_width : float;
    }

    (* from_json *)
    let from_json json =
      { fontname = assoc_string json "fontname" "noname" ;
        line_gap = assoc_float  json "line_gap" 1.0 ;
        ascent = assoc_float  json "ascent" 1.0 ;
        descent = assoc_float  json "descent" 1.0 ;
        line_spacing = assoc_float  json "line_spacing" 1.0 ;
        space_width = assoc_float  json "space_width" 1.0 ;
      }
end

module Outline = struct
    type t = {
    metrics : Metrics.t ;        
    glyphs : (string * Glyph.t) list ;
      }

    (*f load_json *)
    let load_json filename = 
      let json = Yojson.Basic.from_file (filename^".oftj") in
      match json with
        `Assoc fd_g ->
        let font_data  = assoc_assoc json "font_data" [] in
        let glyph_data = assoc_assoc json "glyphs" [] in
        let metrics = Metrics.from_json (`Assoc font_data) in
        let glyph_of_json gn_gd =
          let (name, json_data) = gn_gd in
          (name, Glyph.from_json json_data)
        in
        let glyphs = List.map glyph_of_json glyph_data in
        Some {metrics;glyphs}
      | _ -> None

    (*f get_glyph *)
    let get_glyph ?xsc:(xsc=1.) ?ysc:(ysc=1.) ?xofs:(xofs=0.) ?yofs:(yofs=0.) t glyph_name =
      if (not (List.mem_assoc glyph_name t.glyphs)) then
        None
      else
        let gd = List.assoc glyph_name t.glyphs in
        let metrics = gd.Glyph.metrics in
        let tris    = gd.Glyph.triangles in
        let xmult = xsc /. t.metrics.Metrics.ascent in
        let ymult = ysc /. t.metrics.Metrics.ascent in
        let transform pt = let (x,y) = pt in
                           ((x *. xmult)+.xofs, (y *. ymult)+.yofs)
        in
        let pts = List.map transform gd.Glyph.points in
        Some ((metrics.Glyph.advance_width *. xmult) +. xofs,
              pts,
              tris)

    (* get_glyph_or_blank *)
    let get_glyph_or_blank ?xsc:(xsc=1.) ?ysc:(ysc=1.) ?xofs:(xofs=0.) ?yofs:(yofs=0.) t glyph_name =
        match get_glyph ~xsc:xsc ~ysc:ysc ~xofs:xofs ~yofs:yofs t glyph_name with
          None -> (xofs,[],[])
        | Some d -> d

    (*f get_line_spacing *)
    let get_line_spacing t = t.metrics.Metrics.line_spacing /. t.metrics.Metrics.ascent

    (* All done  *)
end
