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
open Atcflib
open Tgl4
open Result
open Bigarray
open Font
open Utils

(*a OpenGL object classes *)
(*c ogl_obj_text
 Only does zero thickness text at the moment
 *)
class ogl_obj_text ?size:(size=1.0) ?height:(height=1.0) (font:Font.Outline.t) (text:string) =
  object (self)

    (*f subclass of ogl_obj *)
    inherit Obj_base.ogl_obj as super

    val mutable draw_fn  = fun _ -> ();
  (*f build_text - accumulate (ofs, num_pts, pt_list, tri_list) for
  text, one character at at time *)
  val pts_tris =
    let build_text acc c =
      let (xofs, l, pt_list, tri_list) = acc in
      let renumber_tri t = let (a,b,c) = t in (a+l, b+l, c+l) in
      let (new_xofs, pts, tris) = Font.Outline.get_glyph_or_blank ~xofs:xofs ~xsc:size ~ysc:height font c in
      let new_tri_list = tri_list @ (List.map renumber_tri tris)  in
      (new_xofs, (l+(List.length pts)), pt_list @ pts, tri_list @ new_tri_list)
    in
    let rec string_fold ?n:(n=0) f acc text = 
      if n = String.length text then acc else
        let new_acc = f acc (String.make 1 text.[n]) in
        string_fold ~n:(n+1) f new_acc text
    in
    let _,_,pts,tris = string_fold build_text (0.,0,[],[]) text in
    (pts, tris) ;
      
  (*f get_bbox *)
  method get_bbox =
    let (pts, tris) = pts_tris in
    let do_pt acc pt =
      let (x,y) = pt in
      let (x0,x1,y0,y1,z0,z1) = acc in
      ((min x0 x), (max x1 x), (min y0 y), (max y1 y), (min z0 0.), (max z0 0.))
    in
    List.fold_left do_pt (0.,0.,0.,0.,0.,0.) pts
    
  (*f create_geometry - use 0 thickness to start with *)
  method create_geometry ~offset =
    let (pts, tris) = pts_tris in
    let num_pts = List.length pts in
    let num_tris = List.length tris in
    let vertices = ba_float_array (num_pts * 3) in
    let colors   = ba_float_array (num_pts * 3) in
    let indices  = ba_uint16_array (num_tris*3) in
    let (ox,oy,oz) = offset in
    let do_pt i pt =
      let (x,y) = pt in
      vertices.{i*3+0} <- x  +. ox;
      vertices.{i*3+1} <- y  +. oy;
      vertices.{i*3+2} <- 0. +. oz;
      colors.{i*3+0} <- 0. ;
      colors.{i*3+1} <- 1. ;
      colors.{i*3+2} <- 0.
    in
    let do_tri i tri =
      let (a,b,c) = tri in
      indices.{3*i+0} <- a ;
      indices.{3*i+1} <- b ;
      indices.{3*i+2} <- c ;
    in
    List.iteri do_pt pts ;
    List.iteri do_tri tris ;
    super#create_geometry_from_indices indices [vertices; colors] >>= fun _ ->
      let d _ = Gl.draw_elements Gl.triangles (num_tris*3) Gl.unsigned_short (`Offset 0);
                () in
      draw_fn <- d;
      Ok ()

  (*f draw - super's draw with a callback of the create_geometry resultant draw function *)
  method draw view_set other_uids = self#bind_and_draw draw_fn

  (*f All done *)
end

