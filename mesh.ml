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

open Atcflib
open Tgl4
open Result
open Bigarray

(*a Useful functions *)
let epsilon = 1.E-10
let cos_parallel = 0.999999
let cos_angle_between v0 v1 = 
    let c = (Vector.((dot_product v0 v1) /. (modulus v0) /. (modulus v1))) in
    Printf.printf "Cos angle between %s and %s is %f\n%!" (Vector.str v0) (Vector.str v1) c;
    c

let count_elements filter list =
    let count_if acc e = if (filter e) then (acc+1) else acc in
    List.fold_left count_if list

(*f Exceptions *)
exception Point_not_found of string
exception Triangle_not_found of string
exception Line_not_found of string
exception Not_in_line of string
exception Not_in_triangle of string
exception Malformed of string
exception Duplicate_point of string
exception Duplicate_line of string
exception Duplicate_triangle of string
exception Bad_operation of string
exception Too_many_triangles of string

(*f Globals *)

(*f sfmt *)
let sfmt = Printf.sprintf

(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found
(*a Point

    A point within a (contoured) mesh

    A mesh point is a 2d-point (using an instance of a point class) that will be part of the mesh, and it will be part of lines and triangles in the mesh

    A mesh point will be in any number of line segments and triangles, but in a complete mesh every point should be part of at least two lines and one triangle

    The instance of the 'point class' which the mesh point uses must provide:
    * a 'coords' property that is either a tuple or list of two coordinates.
    *)
module rec Point : sig
  (*t Type *)
  type t = {
      pt            : Vector.t;
      line_segments : (Line.t * Point.t * int) list ref ; (* List of lines the point is on *)
      triangles     : Triangle.t list ref; (* List of triangles the point is part of *)
      number        : int option ref; (* Owned by the client *)
      tmp_vector    : Vector.t;
    }

  (*f Functions *)
  val create  : Vector.t -> t
  val check_consistent  : t -> unit
  val coords       : t -> float array
  val set_coord    : t -> int -> float -> unit
  val add_to_line  : t -> Line.t -> t -> int -> unit
  val remove_from_line  : t -> Line.t -> unit
  val used_in_lines  : t -> bool
  val all_lines      : t -> Line.t list
  val is_on_line     : t -> Line.t -> bool
  val add_to_triangle       : t -> Triangle.t -> unit
  val remove_from_triangle  : t -> Triangle.t -> unit
  val find_line_segment_to      : ?mesh_only:bool -> t -> t -> Line.t option
  val find_line_segment_to_exn  : ?mesh_only:bool -> t -> t -> Line.t 
  val find_line_segment_toward  : ?verbose:bool -> t -> Vector.t -> Line.t option
  val compare_with              : t -> t -> int
  val str_of_n                  : t -> string
  val str                       : t -> string
  val str_s                     : t -> string

  (*f All done *)

(*c struct *)
end = struct
  (*t type *)
  type t = {
      pt            : Vector.t;
      line_segments : (Line.t * Point.t * int) list ref ; (* List of lines the point is on *)
      triangles     : Triangle.t list ref; (* List of triangles the point is part of *)
      number        : int option ref; (* Owned by the client *)
      tmp_vector    : Vector.t;
    }

  (*f create *)
  let create pt = { 
      pt = pt;
      triangles = ref [];
      line_segments = ref [];
      number = ref None;
      tmp_vector = Vector.copy pt;
    }

  (*f check_consistent
        Check the mesh point is consistent.

        Checks that every line segment that the mesh point believes it is part of has the mesh point as one of its two end-points.
        Checks that every triangle that the mesh point believes it is part of has the mesh point as one of its three points.
        Checks that there are no duplicate entries in the triangles and line segment lists

   *)
  let check_consistent t = 
    let is_ls_consistent l_pt_n =
      let (l, pt, n) = l_pt_n in
      let is_same_line l_pt_n  =
        let (l2,_,_) = l_pt_n in
        (l2==l)
      in
      let (lpt0, lpt1) = Line.get_points l in
      if ((n=0) && (t != lpt0)) then
        raise (Not_in_line (sfmt "at expected point %d pt [%s] : line [%s]" n (Point.str t) (Line.str l)));
      if ((n=1) && (t != lpt1)) then
        raise (Not_in_line (sfmt "at expected point %d pt [%s] : line [%s]" n (Point.str t) (Line.str l)));
      if ((count_elements is_same_line 0 !(t.line_segments)) > 1) then
        raise (Duplicate_line (sfmt "pt [%s] : line [%s]" (Point.str t) (Line.str l)));
      ()
    in
    let is_tri_consistent tri =
      let (pt0, pt1, pt2) = Triangle.get_points tri in
      let is_same_triangle tri2 = (tri==tri2)
      in
      if ((t != pt0) && (t != pt1) && (t != pt2)) then
        raise (Not_in_triangle (sfmt "pt [%s] : tri [%s]" (Point.str t) (Triangle.str tri)));
      if ((count_elements is_same_triangle 0 !(t.triangles)) > 1) then
        raise (Duplicate_triangle (sfmt "pt [%s] : tri [%s]" (Point.str t) (Triangle.str tri)));
      ()
    in
    List.iter is_ls_consistent !(t.line_segments);
    List.iter is_tri_consistent !(t.triangles);
    ()

  (*f coords - Return the 2D coordinates of the mesh point using the 'point class' instance that it was created with *)
  let coords t = 
    Vector.coords t.pt

  (*f set_coord - coordinates of a point (for fine adjustment only) *)
  let set_coord t n f =
    ignore (Vector.set t.pt n f);
    ()

  (*f add_to_line - Add the mesh point to a line segment by updating the mesh point data *only*. *)
  let add_to_line t line other_pt line_pt_num = 
    t.line_segments := (line, other_pt, line_pt_num) :: !(t.line_segments)

  (*f remove_from_line - Remove the mesh point from a line segment by updating the mesh point data *only*.
        Unlike python, does not
    raise Exception("Failed to find %s in attempting to remove %s from that line"%(line,self))
   *)
  let remove_from_line t line =
    let removed acc l_pt_n = 
      let (l, pt, n) = l_pt_n in
      if (l==line) then acc else (l_pt_n::acc)
    in
    t.line_segments := List.fold_left removed [] !(t.line_segments)

  (*f used_in_lines - Return True if the mesh point is used in any lines
   *)
  let used_in_lines t =
    (List.length !(t.line_segments))>0

  (*f all_lines - Return list of all the line segments the point is on *)
  let all_lines t =
    let add acc l_pt_n = 
      let (l, pt, n) = l_pt_n in (l::acc)
    in
    List.fold_left add [] !(t.line_segments)

  (*f is_on_line - Return True if the mesh point is one of the end-points of the line, else return False.
   *)
  let is_on_line t line = 
    let matches acc l_pt_n = 
      let (l, pt, n) = l_pt_n in
      if (l==line) then true else acc
    in
    List.fold_left matches false !(t.line_segments)

  (*f add_to_triangle - Add the mesh point to a triangle by updating the mesh point data *only* *)
  let add_to_triangle t triangle = 
    t.triangles := triangle :: !(t.triangles)

  (*f remove_from_triangle - Remove the mesh point from a triangle by updating the mesh point data *only* *)
  let remove_from_triangle t triangle =
    let removed acc tri = 
      if (tri==triangle) then acc else (tri::acc)
    in
    t.triangles := List.fold_left removed [] !(t.triangles)

  (*f find_line_segment_to 
    Find the line segment which goes from this mesh point to the :data:`other` mesh point.

        other - the other mesh point in the line segment to find
        mesh_only - True if only points that are in the mesh (i.e. those that are part of triangles) should be searched
   *)
  let find_line_segment_to ?mesh_only:(mesh_only=false) t other  =
    let matches acc l_pt_n = 
      let (l, pt, n) = l_pt_n in
      if (mesh_only && ((Line.num_triangles l)=0)) then acc
      else if (pt==other) then (Some l) else acc
    in
    List.fold_left matches None !(t.line_segments)

  (*f find_line_segment_to_exn
    Find the line segment which goes from this mesh point to the :data:`other` mesh point.

        other - the other mesh point in the line segment to find
        mesh_only - True if only points that are in the mesh (i.e. those that are part of triangles) should be searched
   *)
  let find_line_segment_to_exn ?mesh_only:(mesh_only=false) t other  =
    let line = find_line_segment_to ~mesh_only:mesh_only t other in
    if (option_is_none line) then raise (Line_not_found (sfmt "from pt[%s] towards pt[%s]" (Point.str t) (Point.str other)));
    option_get line

  (*f find_line_segment_toward
    Find the line segment which goes from this mesh point toward the other data point, if there is one

    Do a dot product of (pt - other) with each line direction, noting the point
   *)
  let find_line_segment_toward ?verbose:(verbose=false) t other =
    Printf.printf "find_line_segment_toward pt[%s] vec %s\n%!" (Point.str t) (Vector.str other);
    ignore Vector.(add_scaled (assign t.tmp_vector other) t.pt (-. 1.0));
    let closest_segment acc l_pt_n : (Line.t * float) option =
      let ((l:Line.t), pt, n) = l_pt_n in
      let c = cos_angle_between l.direction t.tmp_vector in
      let rc = if (n==1) then (-. c) else (c) in
      if (option_is_none acc) then Some (l,rc)
      else 
        let (cl, cc) = (option_get acc) in
        if (cc<rc) then Some (l,rc) else acc
    in
    let closest_l_c = List.fold_left closest_segment None !(t.line_segments) in
    if option_is_none closest_l_c then None
    else let (cl, cc) = (option_get closest_l_c) in
         Printf.printf "Closest lc is %f %s\n%!" cc (Line.str cl);
         if (cc<cos_parallel) then None
         else Some cl

  (*f compare_with

        Compare a mesh point with another mesh point in terms of coordinates.

        This permits a kind of ordering for the mesh points, particularly to find the 'leftmost' in some manner.

        Compare the first coordinate - return -1 if this mesh point is less than the other, 1 if greater than, else contine.
        Compare the second coordinate - return -1 if this mesh point is less than the other, 1 if greater than, else contine.
        Return 0 then if the points are equal.
   *)
  let compare_with t other = 
    let l = Vector.length (t.pt) in
    let rec test_coord i =
      if (i=l) then 0 else
        (
          let pc = Vector.get (t.pt) i in
          let oc = Vector.get (t.pt) i in
          let cmp = compare pc oc in
          if (cmp!=0) then cmp else (test_coord (i+1))
        )
    in test_coord 0

  (*f str_of_n
   Provide a string representation of the number
   *)
  (*f str_of_n
   Provide a string representation of the number
   *)
  let str_of_n t =
    if (option_is_none !(t.number)) then "X" else (sfmt "%d" (option_get !(t.number)))

  (*f str
   Provide a string representation of a mesh point including lines and triangles that it is part of
    Use line numbers and triangle numbers, as they are allowed to include the full str of the points in their str functions
   *)
  let str t =
    let str_of_ls l_pt_n acc =
      let (l, pt, n) = l_pt_n in
      (acc ^ (sfmt "%s/%d," (Line.str_of_n l) n)) in
    let str_of_tri tri acc =
      (acc ^ (sfmt "%s," (Triangle.str_of_n tri))) in
    sfmt "pt(%s:%s:(%s):(%s))"
                  (str_of_n t)
                  (Vector.str t.pt)
                  (List.fold_right str_of_ls  !(t.line_segments) "")
                  (List.fold_right str_of_tri !(t.triangles) "")

  (*f str_s
   Provide a short representation of the point
   If the point has an entry number then Pt%d, else its coords
   *)
  let str_s t =
    if (option_is_some !(t.number)) then
      str_of_n t
    else
      sfmt "%s" (Vector.str t.pt)

  (*f All done *)
end

(*a Line
    A line in a mesh, constructed from two mesh points

    A line in a mesh has the following properties:

    :data:`pts`
        Two points as a tuple that are the end-points of the line, in any order.

    :data:`direction`
        Two coordinates containing the vector from pts[0] to pts[1].

    :data:`triangles`
        A 2-tuple of triangles that the line is part of. If the line is part of no triangles, this will be
        (None, None). If the line is part of only one triangle, triangles[1] will be None.

    :data:`line_num`
        An integer for debug during printing, unique to each line
 *)
and Line  : sig
  (*f Type *)
  type t = {
      pts           : (Point.t ref * Point.t ref);
      triangles     : (Triangle.t option * Triangle.t option) ref;
      winding_order : bool option ref;
      direction     : Vector.t;
      tmp_vector    : Vector.t;
      number        : int;
      }
  (*f Functions *)
  val create                : Point.t -> Point.t -> t
  val remove                : t -> unit
  val calculate_direction   : t -> unit
  val check_consistent      : t -> unit
  val get_points            : t -> (Point.t * Point.t)
  val get_length            : t -> float
  val other_end             : t -> Point.t -> Point.t
  val is_parallel_to        : t -> t -> bool
  val num_triangles         : t -> int
  val add_to_triangle       : t -> Triangle.t -> unit
  val remove_from_triangle  : t -> Triangle.t -> unit
  val swap_triangle         : t -> Triangle.t -> Triangle.t -> unit
  val other_triangle        : t -> Triangle.t -> Triangle.t option
  val is_part_of_triangle   : t -> Triangle.t -> bool
  val merge_to_new          : t -> t -> t
  val side_of_line_2d       : ?verbose:bool -> t -> Vector.t -> float
  val set_winding_order     : ?p0:Point.t -> ?p1:Point.t  -> t -> unit
  val get_winding_order     : t -> bool option
  val change_vertex         : t -> Point.t -> Point.t -> unit
  val str_of_n              : t -> string
  val str                   : t -> string

(*c struct *)
end = struct
  (*t type *)
  type t = {
      pts           : (Point.t ref * Point.t ref);
      triangles     : (Triangle.t option * Triangle.t option) ref;
      winding_order : bool option ref;
      direction     : Vector.t;
      tmp_vector    : Vector.t;
      number        : int;
    }
  (*f create
        Create a mesh line from two mesh points

        Ensure the mesh is consistent by adding the new line to the two mesh points
   *)
  let line_log = ref []
  let create pt0 pt1 =
    let number = List.length !line_log in
    let (t:Line.t) = {
        pts = (ref pt0, ref pt1);
        triangles = ref (None, None);
        winding_order = ref None;
        direction = Vector.copy pt0.pt;
        number = number;
        tmp_vector = Vector.copy pt0.pt;
      }
    in
    Line.calculate_direction t ;
    Point.add_to_line pt0 t pt1 0 ;
    Point.add_to_line pt1 t pt0 1 ;
    line_log := t :: (!line_log);
    t

  (*f remove - remove the line consistently

        Note: do not remove from triangles; this is 'downward' in the hierarchy of triangles -> lines -> points

        Henc remove from points associated with the line
   *)
  let remove t =
    let (rpt0, rpt1) = t.pts in
    Point.remove_from_line !rpt0 t;
    Point.remove_from_line !rpt1 t;
    ()

  (*f calculate_direction of the line *)
  let  calculate_direction t =
    let (rpt0, rpt1) = t.pts in
    ignore Vector.(add_scaled (assign t.direction (!rpt1).pt) (!rpt0).pt (-. 1.0));
    ()

  (*f get_points - Get a 2-tuple of the two mesh points at the ends of the line *)
  let get_points t =
    let (rpt0, rpt1) = t.pts in
    (!rpt0, !rpt1)

  (*f get_length  *)
  let get_length t = Vector.modulus t.direction

  (*f other_end *)
  let other_end t pt =
    let (rpt0, rpt1) = t.pts in
    if (pt == !rpt0) then !rpt1 else
      if (pt == !rpt1) then !rpt0 else
        raise (Point_not_found (sfmt "line [%s] : pt [%s]" (Line.str t) (Point.str pt)))

  (*f is_parallel_to Return true if the mesh lines 't' and 'other' are parallel
     This is true if they are going in opposite directions or not *)
  let is_parallel_to t other =
    let c = cos_angle_between t.direction other.direction in
    ((c>cos_parallel) || (c<(-. cos_parallel)))

  (*f num_triangles - return number of triangles the line is part of (0 to 2) *)
  let num_triangles t =
    let (t0, t1) = !(t.triangles) in
    if (option_is_none t0) then 0 
    else if (option_is_none t1) then 1 
    else 2

  (*f add_to_triangle *)
  let add_to_triangle t tri =
    let (t0, t1) = !(t.triangles) in
    if (option_is_none t0) then (t.triangles := (Some tri, None))
    else if (option_is_none t1) then (t.triangles := (t0, Some tri))
    else raise (Too_many_triangles (sfmt "when trying to add to line [%s] to tri [%s]" (Line.str t) (Triangle.str tri)));
    ()

  (*f remove_from_triangle 
        Remove the mesh line from a mesh triangle by updating the mesh line data *only*.
   *)
  let remove_from_triangle t tri =
    let (t0, t1) = !(t.triangles) in
    if ((option_is_some t1) && ((option_get t1)==tri)) then (t.triangles := (t0, None))
    else if ((option_is_some t0) && ((option_get t0)==tri)) then (t.triangles := (t1, None))
    else raise (Triangle_not_found (sfmt "when removing from line [%s] tri [%s]" (Line.str t) (Triangle.str tri)))

  (*f swap_triangle
    Move the mesh line from tri_from to tri_to, but updating the triangles tuple
   *)
  let swap_triangle t tri_from tri_to =
    let (t0, t1) = !(t.triangles) in
    if ((option_is_some t1) && ((option_get t1)==tri_from)) then
      begin
        t.triangles := (t0, Some tri_to)
      end
    else if ((option_is_some t0) && ((option_get t0)==tri_from)) then
      begin
        t.triangles := (Some tri_to, t1)
      end
    else
      raise (Triangle_not_found (sfmt "when swapping in line [%s] tri [%s]" (Line.str t) (Triangle.str tri_from)));
    ()

  (*f other_triangle *)
  let other_triangle t tri =
    let (t0, t1) = !(t.triangles) in
    if ((option_is_some t1) && ((option_get t1)==tri)) then t0
    else if ((option_is_some t0) && ((option_get t0)==tri)) then t1
    else None

  (*f is_part_of_triangle *)
  let is_part_of_triangle t tri =
    let (t0, t1) = !(t.triangles) in
    if ((option_is_some t1) && ((option_get t1)==tri)) then true
    else if ((option_is_some t0) && ((option_get t0)==tri)) then true
    else false

  (*f merge_to_new
        Merge one mesh line segment with another that is in the same direction and has a shared point (e.g. AB, BC merges to AC)

        Cannot be performed it the lines are part of triangles.

        For two mesh lines with a shared point, remove the two lines and return a new line with the non-shared points

        Note that either self.pts[1]==other.pts[0] or self.pts[0]==other.pts[1] (AB, BC or BC, AB)

        After this call the mesh point in the middle may require garbage-collecting from the mesh.
   *)
  let merge_to_new t other = 
    if (((num_triangles t) + (num_triangles other))>0) then
      raise (Bad_operation (sfmt "Attempt to merge lines that are part of triangles tri [%s] tri [%s]" (Line.str t) (Line.str other)));
    let (rpt_a0, rpt_a1) = t.pts in
    let (rpt_b0, rpt_b1) = t.pts in
    Point.remove_from_line !rpt_a0 t;
    Point.remove_from_line !rpt_a1 t;
    Point.remove_from_line !rpt_b0 other;
    Point.remove_from_line !rpt_b1 other;
    let new_line = 
      if (!rpt_a1 == !rpt_b0) then
        (create !rpt_a0 !rpt_b1) (* AB BC -> AC *)
      else
        (create !rpt_b0 !rpt_a1) (* BC AB -> AC *)
    in
    new_line

  (*f side_of_line_2d
    Return <0 if pt is to left of line, >0 if pt is to right of line. *)
  let side_of_line_2d ?verbose:(verbose=false) t pv =
    let (rpt0, rpt1) = t.pts in
    ignore Vector.(add_scaled (assign t.tmp_vector pv) (!rpt0).pt (-. 1.));
    let (dx, dy) = Vector.(get t.tmp_vector 0, get t.tmp_vector 1) in
    ignore Vector.(set (set (scale t.tmp_vector 0.) 0 dy) 1 (-. dx)) ;
    let dot_normal = Vector.dot_product t.tmp_vector t.direction in
    if verbose then
      begin
        Printf.printf "side_of_line_2d : pt[%s] pv [%s]" (Line.str t) (Vector.str pv);
        Printf.printf "side_of_line_2d : dirn [%s]" (Vector.str t.direction);
        Printf.printf "side_of_line_2d : dot_product %f" dot_normal
      end;
    dot_normal

  (*f get_winding_order
   *)
  let get_winding_order t =
    !(t.winding_order)

  (*f set_winding_order
      Set the winding order for the line to be from p0 to p1
   *)
  let set_winding_order ?p0 ?p1 t = 
    if ((option_is_none p0) || (option_is_none p1)) then
      begin
        t.winding_order := None ;
        ()
      end
    else
      let act_p0 = option_get p0 in
      let act_p1 = option_get p1 in
      let (rpt0, rpt1) = t.pts in
      if ((!rpt0 == act_p0) && (!rpt1 == act_p1)) then
        t.winding_order := Some true
      else if ((!rpt0 == act_p1) && (!rpt1 == act_p0)) then
        t.winding_order := Some false
      else
        raise (Point_not_found (sfmt "in setting winding order p0:[%s] p1:[%s]" (Point.str act_p0) (Point.str act_p1)));
      ()

  (*f change_vertex

    Change this line from pt_from/other to be pt_to/other

    This line could be (pt0, pt_from); if so, both pt0 and pt_from
    need to have this line removed and replaced with the true NEW line
    of (pt0, pt_to); similarly if it is (pt_from, pt1)...

   *)
  let change_vertex t pt_from pt_to =
    let (rpt0, rpt1) = t.pts in
    Point.remove_from_line !rpt0 t ; (* remove line,pt_from,0 from pt_other *)
    Point.remove_from_line !rpt1 t ; (* remove line,pt_other,0 from pt_from *)
    if (pt_from==(!rpt1)) then (* line is to be !rpt0, pt_to *)
      begin
        Point.add_to_line  (!rpt0)  t pt_to    0 ;
        Point.add_to_line  pt_to    t (!rpt0)  1 ;
        rpt1 := pt_to ;
      end
    else if (pt_from==(!rpt0)) then (* line is to be pt_to, !rpt1 *)
      begin
        Point.add_to_line  pt_to  t !rpt1  0 ;
        Point.add_to_line  !rpt1  t pt_to  1 ;
        rpt0 := pt_to ;
      end
    else
      raise (Point_not_found (sfmt
                                "Cannot move %s to %s in %s as it is not part of the line"
                                (Point.str pt_from)
                                (Point.str pt_to)
                                (Line.str t)
            ));
    calculate_direction t

  (*f TBD find_intersection_2d Find intersection of the line segment with a line from pt with vector dv

        Intersection is point Z, and this line segment is A to B
        Z = pt + k.dv = l.B + (1-l).A = l.(B-A) + A
        k.dv + l.(A-B) = A-pt

        This can be solved by inverting the matrix
        (A = x0,y0 and B=x1,y1)
        (dxy[0] x0-x1) (k) = ( x0-x )
        (dxy[1] y0-y1) (l)   ( y0-y )

       (k) = (y0-y1   x1-x0 ) (x0-x)    / determinant
       (l)   (-dxy[1] dxy[0]) (y0-y)
    let find_intersection t pt dv =
        epsilon = 1E-9
        (x0,y0) = self.pts[0].coords()
        (x1,y1) = self.pts[1].coords()
        if True:
            if (dxy[0]>0):
                if (x0<pt[0])        and (x1<pt[0]):        return None
                if (x0>pt[0]+dxy[0]) and (x1>pt[0]+dxy[0]): return None
                pass
            else:
                if (x0>pt[0])        and (x1>pt[0]):        return None
                if (x0<pt[0]+dxy[0]) and (x1<pt[0]+dxy[0]): return None
                pass
            if (dxy[1]>0):
                if (y0<pt[1])        and (y1<pt[1]):        return None
                if (y0>pt[1]+dxy[1]) and (y1>pt[1]+dxy[1]): return None
                pass
            else:
                if (y0>pt[1])        and (y1>pt[1]):        return None
                if (y0<pt[1]+dxy[1]) and (y1<pt[1]+dxy[1]): return None
                pass

        det = dxy[0]*(y0-y1) - dxy[1]*(x0-x1)
        if -epsilon<det<epsilon: return None # Parallel lines
        k =  ((y0-y1)*(x0-pt[0]) + (x1-x0)*(y0-pt[1]))/det
        l =  (-dxy[1]*(x0-pt[0]) +  dxy[0]*(y0-pt[1]))/det
        #print k,l
        #print pt[0]+k*dxy[0] - l*x1 - (1-l)*x0
        if k<0 or k>1 or l<0 or l>1: return None
        return (k, l, pt[0]+k*dxy[0], pt[1]+k*dxy[1])
   *)

  (*f check_consistent
        Check that the mesh line is consistent with the rest of the mesh.

        Check that the two mesh points at the ends of the line believe that they are part of this mesh line.
        Check that the triangle 2-tuple is not malformed.
        Check that each triangle which the line is part of both the mesh points that the mesh line has as end-points.
        Check that each triangle finds this line in the mesh when the triangle is asked.
   *)
  let check_consistent t =
    let (rpt0, rpt1) = t.pts in
    let (pt0, pt1) = (!rpt0, !rpt1) in
    let (t0, t1) = !(t.triangles) in
    let diff = Vector.(add_scaled (add_scaled (copy pt1.pt) pt0.pt (-. 1.)) t.direction (-. 1.)) in
    let error = (Vector.modulus diff) in
    let check_triangle_consistent opt_tri =
      if (option_is_none opt_tri) then ()
      else
        let tri = option_get opt_tri in
        let (tpt0, tpt1, tpt2) = Triangle.get_points tri in
        if (not ((pt0==tpt0) || (pt0==tpt1) || (pt0==tpt2))) then
          raise (Point_not_found (sfmt
                                    "point of line not in line's triangle pt[%s] line [%s] tri [%s]"
                                    (Point.str pt0)
                                    (Line.str t)
                                    (Triangle.str tri)));
        if (not ((pt1==tpt0) || (pt1==tpt1) || (pt1==tpt2))) then
          raise (Point_not_found (sfmt
                                    "point of line not in line's triangle pt[%s] line [%s] tri [%s]"
                                    (Point.str pt1)
                                    (Line.str t)
                                    (Triangle.str tri)));
         let tri_lines = Triangle.find_lines_of_points ~mesh_only:true tri in
         let line_find acc l = (acc || (t==l)) in
         if (not (List.fold_left line_find false tri_lines)) then
           raise (Triangle_not_found (sfmt
                                        "line [%s] was not the line for one of its triangles [%s]"
                                        (Line.str t)
                                        (Triangle.str tri)));
         ()
    in
    if (pt0 == pt1) then
      raise (Duplicate_point (sfmt ""));
    if (not (Point.is_on_line pt0 t)) then
      raise (Point_not_found (sfmt ""));
    if (not (Point.is_on_line pt1 t)) then
      raise (Point_not_found (sfmt ""));
    if ((option_is_none t0) && (option_is_some t1)) then
      raise (Malformed (sfmt
                          "Triangle pair of line has t0 None, t1 non-None line:[%s]"
                          (Line.str t)
            ));
    check_triangle_consistent t0;
    check_triangle_consistent t1;    
    if (error>epsilon) then
      raise (Malformed (sfmt "Bad direction (error %f) line:[%s]" error (Line.str t)));
    ()

  (*f TBD can_shorten_diagonal
    def can_shorten_diagonal( self ):
        """
        Determine if this line, being part of two triangles hence forming the diagonal a quadrilateral, would be shorter if it were the other diagonal of the quadrilateral.

        If the mesh line is not part of two triangles, then clearly it cannot be a shorter diagonal.
        If the quadrilateral is concave then the 'other diagonal' will not work (even if shorter) as it would be outside the quadrilateral

        To check for concavity, we know that the quad (ABCD, for quad[0..3]) has a good diagonal AC (this mesh line).
        Now, looking at BD (the other diagonal), we can see that the quadrilateral is concave if both A and C are on the same side of BD.
        We calculate the vector BD as dx1,dy1; the normal (it does not matter which way) is dy1,-dx1.
        The scalar product of AB with the normal to BD is |AB|.|BD_normal|.cos(ABD); the sign of this indicates which side of BD point A is.
        The scalar product of CB with the normal to BD is |CB|.|BD_normal|.cos(CBD); the sign of this indicates which side of BD point C is.
        So if these are multiplied, the result is positive if both points are on the same side, negative if they are on opposite sides, and zero if at least one point is on the line.

        Here 'on the line' is not yet clear - it is best to avoid such circumstances.
        """
        #b Check that there are two triangles
        if self.triangles[1] is None: return False

        #b Get the coordinates (and for humans, the quadrilateral...)
        quad = [self.pts[0].coords(), None, self.pts[1].coords(), None]
        quad[1] = self.triangles[0].get_other_point( self.pts ).coords()
        quad[3] = self.triangles[1].get_other_point( self.pts ).coords()

        #b Check to see if diagonal is shorter - if not, then return False
        (dx0, dy0) = ( quad[2][0]-quad[0][0], quad[2][1]-quad[0][1] )
        (dx1, dy1) = ( quad[3][0]-quad[1][0], quad[3][1]-quad[1][1] )
        if (dx0*dx0+dy0*dy0 <= dx1*dx1+dy1*dy1): return False

        #b Are both quad[0] and quad[2] on the same side of the line quad[3]-quad[1]? If so, concave :-(
        if ( ( dy1*(quad[0][0]-quad[1][0])-dx1*(quad[0][1]-quad[1][1]) ) *
             ( dy1*(quad[2][0]-quad[1][0])-dx1*(quad[2][1]-quad[1][1]) ) ) >= 0: return False
        return True *)

  (*f TBD swap_diagonal
    def swap_diagonal( self, mesh, verbose=False ):
        """
        Trusting that the line has two triangles (ABX and ABY) move it to be XY and the triangles to be AYX and XBY

        For consistency, the line has to be removed from points A and B
        Point B has to be removed from the first triangle
        Point A has to be removed from the second triangle
        Point Y has to be added to the first triangle
        Point X has to be added to the second triangle
        The line is added to points X and Y (line 'XY')
        The line is changed to be X and Y
        The first triangle changes its points from ABX to AXY
        The second triangle changes its points from ABY to XBY
        """
        if self.triangles[1] is None: die_horribly
        A = self.pts[0]
        B = self.pts[1]
        X = self.triangles[0].get_other_point( self.pts )
        Y = self.triangles[1].get_other_point( self.pts )
        if verbose:
            mesh.check_consistent()
            print "********************************************************************************"
            print mesh.__repr__(verbose=True)
            print "********************************************************************************"
            print "Swapping diagonal (A,B) with (X,Y) for line",A,B,X,Y,self
            print "Triangles ",self.triangles[0], self.triangles[1]
        self.pts = ( X, Y )
        self.calculate_direction()
        self.triangles[0].change_point_on_diagonal( B, Y, self.triangles[1] )
        self.triangles[1].change_point_on_diagonal( A, X, self.triangles[0] )
        if verbose:
            print "Now", A,B,X,Y,self
            print "Triangles ",self.triangles[0], self.triangles[1]
            pass
        A.remove_from_line( self )
        B.remove_from_line( self )
        X.add_to_line( self, Y, 0 )
        Y.add_to_line( self, X, 1 )
        if verbose:
            print "********************************************************************************"
            print mesh.__repr__(verbose=True)
            print "********************************************************************************"
            mesh.check_consistent()
            pass
        pass *)

  (*f str_of_n
   Provide a string representation of the number
   *)
  let str_of_n t =
    sfmt "%d" t.number

  (*f str *)
  let str t =
    let (rpt0, rpt1) = t.pts in
    let winding = 
      if (option_is_some !(t.winding_order)) then
        (sfmt "%b" (option_get !(t.winding_order)))
      else
        "None"
    in
    let (t0, t1) = !(t.triangles) in
    let triangle opt_tri =
      if (option_is_none opt_tri) then "None" else (Triangle.str_of_n (option_get opt_tri))
    in
    let triangles = sfmt "(%s, %s)" (triangle t0) (triangle t1)
    in
    sfmt "line(#%s:w %s:p0 %s:p1 %s:t %s)" (Line.str_of_n t) winding (Point.str_s (!rpt0)) (Point.str_s (!rpt1)) triangles

  (*f All done *)
end

(*a Triangle
    A triangle in the mesh contains three points, and implicitly three line segments.

    It is created from the three points, assuming that the lines have already been made.
    The points are c_mesh_point instances
 *)
and Triangle  : sig
  (*t Type *)
  type t = {
      pts           : (Point.t ref * Point.t ref * Point.t ref); (* Points of the triangle *)
      winding_order : int option ref;   (* None if has no order yet *)
      number        : int;              (* Number of triangle, monotonically increasing for each new triangle *)
    }

  (*f Functions *)
  val create                 : Point.t -> Point.t -> Point.t -> t
  val remove                 : t -> unit
  val check_consistent       : t -> unit
  val nth_point              : t -> int -> Point.t
  val get_other_point        : t -> Point.t -> Point.t -> Point.t option
  val get_other_point_exn    : t -> Point.t -> Point.t -> Point.t
  val get_points             : t -> (Point.t * Point.t * Point.t)
  val find_lines_of_points   : ?mesh_only:bool -> t -> Line.t list
  val change_vertex          : t -> Point.t -> Point.t -> unit
  val move_point_to_pts_0    : t -> Point.t -> unit
  val change_point_on_diagonal  : t -> Point.t -> Point.t -> t -> unit
  val get_area_2d               : t -> float
  val set_winding_order         : ?winding_order_base:int -> ?verbose:bool -> opt_line:Line.t option -> t -> (Line.t * Line.t) option
  val str_of_n                  : t -> string
  val str                       : t -> string

  (*f All done *)

(*c struct *)
end = struct 
  (*t type *)
  type t = {
      pts           : (Point.t ref * Point.t ref * Point.t ref); (* Points of the triangle *)
      winding_order : int option ref;   (* None if has no order yet *)
      number        : int;              (* Number of triangle, monotonically increasing for each new triangle *)
    }

  (*v Statics *)
  let triangle_log = ref []

  (*f create To create a triangle, the lines between the points must have already been created so that the points are connected*)
  let create pt0 pt1 pt2 = 
    let number = List.length !triangle_log in
    let t = {
        pts = (ref pt0, ref pt1, ref pt2);
        winding_order = ref None;
        number = number;
      } in
    triangle_log := t :: (!triangle_log) ;
    Point.add_to_triangle pt0 t;
    Point.add_to_triangle pt1 t;
    Point.add_to_triangle pt2 t;
    let l0 = Point.find_line_segment_to_exn pt0 pt1 in
    let l1 = Point.find_line_segment_to_exn pt1 pt2 in
    let l2 = Point.find_line_segment_to_exn pt2 pt0 in
    Line.add_to_triangle l0 t ;
    Line.add_to_triangle l1 t ;
    Line.add_to_triangle l2 t ;
    t

  (*f remove - Remove a triangle from its associated points and lines *)
  let remove t = 
    let (rpt0, rpt1, rpt2) = t.pts  in
    let l0 = Point.find_line_segment_to_exn !rpt0 !rpt1 in
    let l1 = Point.find_line_segment_to_exn !rpt1 !rpt2 in
    let l2 = Point.find_line_segment_to_exn !rpt2 !rpt0 in
    Line.remove_from_triangle l0 t ;
    Line.remove_from_triangle l1 t ;
    Line.remove_from_triangle l2 t ;
    Point.remove_from_triangle !rpt0 t ;
    Point.remove_from_triangle !rpt1 t ;
    Point.remove_from_triangle !rpt2 t ;
    ()

  (*f check_consistent
      Check that the triangle is consistent within the mesh.

      The triangle is inconsistent if some of the lines in the mesh, derived from the points in the triangle, do not believe they are part of this triangle
   *)
  let check_consistent t =
    let validate_line_in_triangle l =
      if (not (Line.is_part_of_triangle l t)) then
        raise (Line_not_found (sfmt
                                 "line is not part of triangle for tri [%s]: line [%s]"
                                 (Triangle.str t)
                                 (Line.str l)
              ));
      ()
    in
    List.iter validate_line_in_triangle (Triangle.find_lines_of_points ~mesh_only:true t)

  (*f nth_point get nth point *)
  let nth_point t n =
    let (rpt0, rpt1, rpt2) = t.pts  in
    match n with
      0 -> !rpt0
    | 1 -> !rpt1
    | _ -> !rpt2

  (*f get_other_point
    Get the point in the triangle that is not pt0 or pt1
   *)
  let get_other_point t pt0 pt1 = 
    let (rpt0, rpt1, rpt2) = t.pts in
    if      ((not (!rpt0 == pt0)) && (not (!rpt0 == pt1))) then (Some !rpt0)
    else if ((not (!rpt1 == pt0)) && (not (!rpt1 == pt1))) then (Some !rpt1)
    else if ((not (!rpt2 == pt0)) && (not (!rpt2 == pt1))) then (Some !rpt1)
    else None

  (*f get_other_point_exn
    Get the point in the triangle that is not pt0 or pt1
   *)
  let get_other_point_exn t pt0 pt1 =
    let pt = get_other_point t pt0 pt1 in
    if (option_is_none pt) then raise (Point_not_found "");
    option_get pt    

  (*f get_points

    Get the points in the triangle
   *)
  let get_points t =
    let (rpt0, rpt1, rpt2) = t.pts in
    (!rpt0, !rpt1, !rpt2)

  (*f find_lines_of_points - return tuple of the three lines that make up the triangle from its points
    Find the three lines that make up the triangle, using the triangles mesh points and finding the lines that they form.
   *)
  let find_lines_of_points ?mesh_only:(mesh_only=false) t = 
    let (pt0, pt1, pt2) = get_points t in
    let l0 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt0 pt1 in
    let l1 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt1 pt0 in
    let l2 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt0 pt2 in
    let l3 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt2 pt0 in
    let l4 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt1 pt2 in
    let l5 = Point.find_line_segment_to_exn ~mesh_only:mesh_only pt2 pt1 in
    if ( not ((l0==l1) && (l2==l3) && (l4==l5)) ) then
      raise (Malformed (sfmt
                          "Two different lines between same two points in tri:[%s]"
                          (Triangle.str t)
            ));
    if ( (l0==l2) || (l2==l4) || (l4==l0) ) then
      raise (Duplicate_line (sfmt
                               "Two lines same int tri:[%s]"
                               (Triangle.str t)
            ));
    [l0; l2; l4]

  (*f change_vertex WITHOUT TOUCHING THE LINE DATA

    Change a vertex for a triangle and update the mesh point data

    The triangle is A, B, F and has lines AB, AF, BF

    Afterwards the triangle is A, B, T and the lines are still AB, AF, BF

    Hence the line AF has to be changed in some other way to be AT, and BF to be BT
   *)
  let change_vertex t pt_from pt_to =
    let (rpt0, rpt1, rpt2) = t.pts in
    let rpt =
     if ((!rpt0)==pt_from) then rpt0 else
     if ((!rpt1)==pt_from) then rpt1 else
     if ((!rpt2)==pt_from) then rpt2 else
     raise (Point_not_found (sfmt "changing vertex pt:[%s] in tri:[%s]" (Point.str pt_from) (Triangle.str t)));
    in
    Point.remove_from_triangle !rpt t ;
    rpt := pt_to ;
    Point.add_to_triangle      !rpt   t

  (*f move_point_to_pts_0 - Change the list of points to have pt as index 0 of t.pts *)
  let move_point_to_pts_0 t pt =
    let (rpt0, rpt1, rpt2) = t.pts in
    let (pt0, pt1, pt2) = (!rpt0, !rpt1, !rpt2) in
    if (!rpt0 == pt) then ()
    else if (!rpt1 == pt) then (rpt0:=pt1; rpt1:=pt2; rpt2:=pt0; ())
    else if (!rpt2 == pt) then (rpt0:=pt2; rpt1:=pt0; rpt2:=pt2; ())
    else raise (Point_not_found (sfmt "in moving to pt0 in tri[%s] pt[%s]" (Triangle.str t) (Point.str pt)))

  (*f change_point_on_diagonal (quadrilateral ABFT as ABF/AFT becomes ABT/BFT)

        Change a point 'F' from this triangle to be point 'T' consistently,
        does NOT effect the other triangle.

        The five lines to start with AB, BF, FT, AT, and the common line AF
        The five lines a full swap of the diagonal are AB, AT, BF, FT, and the common line BT
        The lines AB and FT remain in the triangles they were in.
        The line AT is in the other triangle to start and will move to this one
        The line BF is in this triangle to start and will move to the other one
        The common line AF becomes the new common line BT that is in both triangles
        This method takes charge of only changing line BF to believe it is no longer in this triangle, but in other

        Find the point 'pt_from' (F) in the triangle's list of points
        Find the lines 'AF' and 'BF', and if that is in this triangle but not the other (it is BF) then change the line to be part of the other triangle.
        This is done for consistency.
        Then remove F from this triangle and add T to this triangle
   *)
  let change_point_on_diagonal t pt_from pt_to  other_t =
    (* Make triangle t be FAB *)
    move_point_to_pts_0 t pt_from;
    let (rpt0, rpt1, rpt2) = t.pts in
    (* Find lines FA and FB *)
    let fa = Point.find_line_segment_to_exn pt_from (!rpt1) ~mesh_only:true in
    let fb = Point.find_line_segment_to_exn pt_from (!rpt2) ~mesh_only:true in
    (* If FA is part of this but not other (i.e. other is AFT)...  *)
    if ((Line.is_part_of_triangle fa t) && not ((Line.is_part_of_triangle fa other_t))) then
      Line.swap_triangle fa t other_t;
    if ((Line.is_part_of_triangle fb t) && not ((Line.is_part_of_triangle fb other_t))) then
      Line.swap_triangle fb t other_t;
    Point.remove_from_triangle pt_from t;
    Point.add_to_triangle pt_to t;
    rpt0 := pt_to

  (*f get_area_2d - for 2D vectors for now
     Return the area of the triangle
     *)
  let get_area_2d t = 
    let (rpt0, rpt1, rpt2) = t.pts in
    let (x0,y0) = Vector.(get (!rpt0).pt 0, get (!rpt0).pt 1) in
    let (x1,y1) = Vector.(get (!rpt1).pt 0, get (!rpt1).pt 1) in
    let (x2,y2) = Vector.(get (!rpt2).pt 0, get (!rpt2).pt 1) in
    let area = (x1-.x0)*.(y2-.y0) -. (x2-.x0)*.(y1-.y0) in
    if (area<0.) then
      (-. area)
    else
      area

  (*f set_winding_order
        Set the winding order for the triangle based on a line and a winding order for the other side of the line
        ..verbose = (self.triangle_num==41) or (self.triangle_num==25)..
   *)
  let set_winding_order ?winding_order_base:(wob=0) ?verbose:(verbose=false) ~opt_line t =
    if (option_is_some !(t.winding_order)) then None (* If there already is a winding order, then we are done *)
    else if (option_is_none opt_line) then (t.winding_order := None; None)
    else
      let line = (option_get opt_line) in
      begin
        let (pt0, pt1) = Line.get_points line in
        let p = get_other_point_exn t pt0 pt1 in
        if verbose then begin
            Printf.printf "swo: pt0[%s] pt1[%s]\n" (Point.str pt0) (Point.str pt1);
            Printf.printf "swo: line[%s]\n" (Line.str line);
          end;
        let line_winding_order = (Line.get_winding_order line) in
        let winding_order = 
          if (option_is_none line_winding_order) then wob
          else (
            let side_of_line = Line.side_of_line_2d ~verbose:verbose line p.pt  in
            if      ((side_of_line<0.) && (option_get line_winding_order)) then (wob-1)
            else if ((side_of_line>0.) && (not (option_get line_winding_order))) then (wob-1)
            else (wob+1)
          )
        in
        let l0 = Point.find_line_segment_to_exn pt0 p in
        let l1 = Point.find_line_segment_to_exn pt1 p in
        t.winding_order := Some winding_order ;
        Some (l0, l1)
      end

  (*f str_of_n
   Provide a string representation of the number
   *)
  let str_of_n t =
    sfmt "%d" t.number

  (*f str
Return a representation of the triangle
   *)
  let str t =
    let winding = 
      if (option_is_none !(t.winding_order)) then "X"
      else sfmt "%d" (option_get !(t.winding_order))
    in
    sfmt "tri(%d:%s,%s,%s,%s,%6.2f)"
         t.number
         winding
         (Point.str_s (nth_point t 0))
         (Point.str_s (nth_point t 1))
         (Point.str_s (nth_point t 2))
         (get_area_2d t)

  (*f All done *)
end

(*a Mesh
    A mesh is a set of mesh points, which are connected by mesh line segments, which are then filled with non-overlapping mesh triangles.

    It can be created from a set of contours, from which a set of mesh points is generated.
    Mesh lines are then created for the points to enable it to be filled with triangles, and triangles are made to fill the convex hull of the mesh points.
    Then the contours can be applied to ensure that all of the contours are present as mesh line segments.

    After the mesh is fully built with contours winding rules and fill can be applied, or gradients applied to lines.

    The mesh always has a list of points (self.point_set), list of line segments, and a list of triangles
    This is a list of c_mesh_points
    Each mesh point has a list of lines it is part of
    The list of line_segments is a list of c_mesh_lines(pta,ptb)
    Each mesh line has a pts=(pta,ptb) tuple attribute and a (tria,trib) triangle tuple
    The triangle list is a list of c_mesh_triangle()
    Each triangle has a list of 3 points, triangle number (unique id), winding order (None, True or False, for whether it is in/out or unknown)

    To use a mesh with contours:
    x = c_mesh()
    x.add_contour( [c_point,...] )
    x.map_contours_to_mesh()
    x.normalize() - number the points, and remove empty triangles

    To create a mesh for a shape from a set of points
    x = c_mesh()
    x.from_points( [c_point,...] )
 *)
module Mesh = struct
  (*t Type *)
  type t = { 
      points    : Point.t list ref;
      lines     : Line.t list ref;
      triangles : Triangle.t list ref;
    }                                                                                                     

  (*f create *)
  let create () = 
    { points=ref [];
      lines = ref [];
      triangles = ref [];
    }

  (*f reset *)
  let reset t = 
    t.points := [];
    t.lines := [];
    t.triangles :=  []
    (*    self.contours = []*)

  (*f check_consistent *)
  let check_consistent t =
    List.iter Point.check_consistent !(t.points);
    List.iter Line.check_consistent !(t.lines);
    List.iter Triangle.check_consistent !(t.triangles);
    ()

  (*f remove_lines *)
  let remove_lines t =
    List.iter Line.remove !(t.lines);
    t.lines := []

  (*f remove_triangles *)
  let remove_triangles t =
    List.iter Triangle.remove !(t.triangles);
    t.triangles := []

  (*f remove_point
        Remove a point from the mesh - just from the mesh data
  *)
  let remove_point t pt =
    let remove acc poss_pt = 
      if poss_pt == pt then acc else (poss_pt::acc)
    in
    t.points := List.fold_left remove [] !(t.points)

  (*f remove_line
        Remove a line to the mesh - just from the mesh data
   *)
  let remove_line t line =
    let remove acc poss_ln = 
      if poss_ln == line then acc else (poss_ln::acc)
    in
    t.lines := List.fold_left remove [] !(t.lines)

  (*f remove_triangle
    Remove a triangle from the mesh - just from the mesh data
   *)
  let remove_triangle t tri = 
    let remove acc poss_tri = 
      if poss_tri == tri then acc else (poss_tri::acc)
    in
    t.triangles := List.fold_left remove [] !(t.triangles)

  (*f add_point_from_vector
        Add a point to the set of points in the mesh

        point is NOT a mesh point
        Add an external representation of a point to the set of points in the mesh, and return that object
        If the point is already in our set, return that object
        Keep the point set ordered by coords
   *)
  let add_point_from_vector ?append_to_numbers:(atn=false) t pv =
    (*(ins, match) = self.find_insertion_index( self.point_set, point, lambda s,e:s.compare_with(e) )
        if match: return self.point_set[ins]
        new_point = c_mesh_point( point )
        self.point_set.insert( ins, new_point )
        if append_to_numbers:
            new_point.entry_number = len(self.point_set)
            pass
        return new_point*)
    let point = Point.create pv in
    t.points := point :: !(t.points);
    point

  (*f add_line 
        Add a line to the mesh using two mesh points.
   *)
  let add_line t pt0 pt1 =
    let line = Line.create pt0 pt1
    in
    t.lines := line :: !(t.lines);
    line

  (*f find_or_create_line
   *)
  let find_or_create_line t pt0 pt1 =
    let opt_line = Point.find_line_segment_to pt0 pt1
    in
    if (option_is_none opt_line) then
      (add_line t pt0 pt1)
    else
      (option_get opt_line)
        
  (*f add_triangle_from_points
        Must add a triangle from points to ensure that the winding order is passed in cleanly
   *)
  let add_triangle_from_points t pt0 pt1 pt2 =
    find_or_create_line t pt0 pt1;
    find_or_create_line t pt1 pt2;
    find_or_create_line t pt2 pt0;
    let triangle = Triangle.create pt0 pt1 pt2
    in
    t.triangles := triangle :: !(t.triangles);
    triangle

  (*f
    #f find_insertion_index
    def find_insertion_index( self, set, element, compare_fn ):
        """
        Find where to insert an element in a set, using a specified compare_fn

        compare_fn takes (set_element, element) and returns -1 if set_element<element, 0 if set_element==element, 1 if set_element>element

        Return (index, match); if match is True then element compares to match (compare_fn returned 0)
        index is the index of the entry in 'set' to insert 'element' before to maintain an ordered list (i.e. set[index-1]<element<set[index])

        Use a binary search
        """
        l = len(set)
        if l==0: return (0, False)
        (i0, i1) = (-1,l)
        while i0<i1-1:
            im = (i0+i1)/2
            cmp = compare_fn( set[im], element )
            if cmp==0:
                return (im, True)
            if cmp<0: # i.e. im is 'less' than element
                i0 = im
                pass
            else:
                i1 = im
                pass
            pass
        return (i1, False)
    #f add_contour
    def add_contour( self, pts, closed=True, contour_data=None ):
        """
        Add a contour to the mesh

        The points must be a list of 'point class' instances which are joined by line segments that make the contour
        The contour may be closed (last point has a line segment to the first point) or open (effectively just a line)
        The contour_data is opaque to the mesh; it can include contour height, winding order, etc, depending on the mesh usage.

        The contour is just added to the mesh; later method invocations are required to create the mesh points and lines
        """
        self.contours.append( {"pts":pts, "mesh_pts":[], "closed":closed, "data":contour_data} )
        pass
    #f add_bezier_list_contour
    def add_bezier_list_contour( self, bezier_list, closed=False, contour_data=None, straightness=1000, perturbation=None ):
        points = []
        i = 0
        for b in bezier_list:
            subbeziers = b.break_into_segments(straightness)
            for s in subbeziers:
                if perturbation is not None:
                    s.pts[0].perturb(i*perturbation)
                    pass
                points.append(s.pts[0])
                i += 1
                pass
            pass
        self.add_contour( points, closed=closed, contour_data=contour_data )
        return self
    #f map_contours_to_mesh
    def map_contours_to_mesh( self ):
        """
        Map the contours of the mesh to mesh points

        Add all the points as mesh points, and update the contour.
        """
        for c in self.contours:
            mesh_pts = c["mesh_pts"]
            for p in c["pts"]:
                mesh_pts.append( self.add_point( p ) )
                pass
            pass
        pass
    #f from_points
    def from_points( self, points ):
        """
        points is a list of c_points

        Must create the set of c_mesh_point, and the line segments that use them
        The points list can then be sorted to start with the smallest X point (with smallest Y on ties)

        Once sorted, one can reorder the line segments to start at that smallest X point.
        This is a normalized mesh
        """
        self.line_segments = []
        p0 = points[-1]
        for i in range(len(points)):
            p1 = points[i]
            self.add_line( p0, p1 )
            p0 = p1
            pass
        pass
    #f from_bezier_list
    def from_bezier_list( self, bezier_list, straightness=1000 ):
        epsilon = 1e-9
        self.reset()
        points = []
        i = 0
        for b in bezier_list:
            subbeziers = b.break_into_segments(straightness)
            for s in subbeziers:
                s.pts[0].perturb(i*epsilon)
                points.append(self.add_point(s.pts[0]) )
                i += 1
                pass
            pass

        self.from_points( points )
        return self
    #f number_points
    def number_points( self ):
        for i in range(len(self.point_set)):
            self.point_set[i].entry_number = i
            pass
        pass
    #f find_first_segment
    def find_first_segment( self ):
        first_segment = None
        for i in range(len(self.line_segments)):
            if self.line_segments[i].pts[0].entry_number==0:
                return i
                break
            pass
        return None
    #f normalize
    def normalize( self ):
        self.number_points()
        self.remove_empty_triangles()
        pass
    #f split_line_segment_in_half
    def split_line_segment_in_half( self, line ):
        (p0,p1) = line.get_points()
        midpoint = bezier.c_point( coords=( 0.5*(p0.coords()[0]+p1.coords()[0]),
                                            0.5*(p0.coords()[1]+p1.coords()[1]) ) )
        pt = self.add_point(midpoint, append_to_numbers=True)
        self.split_line_segment( line, pt )
    #f split_line_segment
    def split_line_segment( self, line, pt, verbose=False ):
        """
        Split a mesh line segment by inserting a mesh point, which should be on the line

        Return the two mesh line segments that replace the original.

        Effectively replace the line with two line segments, plus up to two further line segments if splitting up to 2 triangles, and create up to 2 more triangles.

        For the line, remove the line from the two mesh points at its ends (P0, P1), and create two more line segments L0=(P0,pt) and L1=(pt,p1)

        For each triangle T (P0.P1.X) in the original line's triangles:
            Move T to be P0.Pt.X (change the point P1 on the triangle to Pt, remove T from P1's triangles, add to Pt's triangles)
            Remove triangle T from line P1.X's triangle list
            Add T to L0's triangles list
            Add line LX (Pt.X), and add T to that line
            Add (new) triangle Pt.P1.X to the mesh

        Return (L0, L1)
        """
        if verbose:
            self.check_consistent()
            pass
        (p0, p1) = line.get_points()
        if (pt is p0) or (pt is p1):
            raise Exception("Request to split a line %s at one of its endpoints %s"%(str(line),str(pt)))
        if (pt.find_line_segment_to(p0)):
            raise Exception("Request to split a line %s at %s when there is already a line from there to one end of the line %s from %s"%(str(line),str(pt),str(p0),str(p1)))
        if (pt.find_line_segment_to(p1)):
            raise Exception("Request to split a line %s at %s when there is already a line from there to one end of the line %s from %s"%(str(line),str(pt),str(p1),str(p0)))
        p0.remove_from_line( line )
        p1.remove_from_line( line )
        l0 = self.add_line( p0, pt )
        l1 = self.add_line( pt, p1 )

        if verbose:
            print "Split line",line,pt
            print "p0",p0
            print "p1",p1
            print "l0",l0
            print "l1",l1
        for t in line.triangles:
            if t is None: continue
            x = t.get_other_point( p1, p0 )
            t.change_vertex( p1, pt )
            p1.find_line_segment_to( other=x, mesh_only=True ).remove_from_triangle( t )
            l0.add_to_triangle( t )
            lx = self.add_line( pt, x )
            lx.add_to_triangle( t )
            self.add_triangle_from_points( (pt, p1, x) )
            pass

        self.remove_line( line )
        if verbose:
            self.check_consistent()
            pass
        return (l0, l1)
    #f remove_empty_triangles
    def remove_empty_triangles( self ):
        """
        Find all pair of consecutive line segments make a 'zero area' triangle, and merge to a single line segment

        If the line segments are (a, b) and (b,c), and b-a and c-b are parallel, then change first line segment to be (a,c) and remove the second
        """
        l = len(self.line_segments)
        i = l-1
        while i>0:
            if self.line_segments[i].is_parallel_to(self.line_segments[(i+1)%l]):
                self.line_segments[i] = self.line_segments[i].merge_to_new(self.line_segments[(i+1)%l])
                old_point = self.line_segments.pop((i+1)%l)
                if not old_point.used_in_lines():
                    self.remove_unused_point( old_point )
                    pass
                if (i+1==l): i-= 2
                l -= 1
                pass
            else:
                i -= 1
                pass
            pass
        pass
    #f fill_convex_hull_with_triangles
    def fill_convex_hull_with_triangles( self, must_be_used_in_lines=False ):
        """
        Starting with a normalized mesh, we can generate filled triangles

        A normalized mesh has a sorted set of points and a (possibly
        empty) list of line segments starting at any point without any
        consecutive parallel line segments Since we can guarantee that
        no consecutive line segments are parallel, two consecutive
        line segments must form a non-zero area triangle The line
        segments can be non-empty so that contours are guaranteed to
        have lines in the final mesh. If there are no contours the mesh
        can start off being just points.

        Sort all points after the 'first point (x0,y0) (left-most, top-most)' by angle to y=y0 (-90,+90).
        The sweep all points creating triangles from (x0,y0) to (xn,yn), (xn+1,yn+1) in the swept order
        """
        radial_order = []
        (x0,y0) = self.point_set[0].coords()
        for pt in self.point_set[1:]:
            if must_be_used_in_lines and not pt.used_in_lines(): continue
            (x,y) = pt.coords()
            (dx,dy) = (x-x0,y-y0)
            (ins, match) = self.find_insertion_index( radial_order, (dx,dy), lambda s,e:s[0]*e[1]-s[1]*e[0] )
            radial_order.insert( ins, (dx, dy, pt) )
            pass
        radial_order.insert(0,(0,0,self.point_set[0]))
        num_points_used = len(radial_order)
        if num_points_used<3:
            return
        # First fill from the 'starter point'
        for i in range(2,num_points_used):
            self.add_triangle_from_points( (radial_order[0][2],radial_order[i-1][2], radial_order[i][2]) )
            pass

        # Now fill in the perimeter (Graham Scan)
        hull_pts = [ radial_order[0][2], radial_order[1][2], radial_order[2][2] ]
        next_point = 3
        while next_point<num_points_used:
            # Check for concavity from (hull_pts[-2], hull_pts[-1]) and (hull_pts[-1],next_point)
            # If not, can add next_point to the hull
            # If there is, then:
            #      add a triangle to fill in the concavity
            #      pop hull_pts[-1] from the points on the hull
            is_concavity = True
            if (len(hull_pts)<2):
                is_concavity = False
                pass
            else:
                (x0,y0) = hull_pts[-2].coords()
                (x1,y1) = hull_pts[-1].coords()
                (x2,y2) = radial_order[next_point][2].coords()
                (dx1,dy1) = (x1-x0,y1-y0)
                (dx2,dy2) = (x2-x0,y2-y0)
                is_concavity = ((dx1*dy2-dy1*dx2)>0)
                pass
            if is_concavity:
                self.add_triangle_from_points( (hull_pts[-2], hull_pts[-1], radial_order[next_point][2] ) )
                hull_pts.pop()
                pass
            else:
                hull_pts.append( radial_order[next_point][2] )
                next_point = next_point+1
                pass
            pass
        # Remove any zero-area triangles which 
        #for l in self.line_segments:
        #    tris = l.get_triangles()
        #    for t in tris:
        #        if t.is_degenerate_with_longest_side(l):
        #            l.swap_as_diagonals()
        #            break
        #        pass
        #    pass

        # Now ensure that the actual lines forming the mesh are in the set of lines
        # tricky
        pass
    #f merge_two_line_ends
    def merge_two_line_ends( self, line, verbose=False ):
        """
        The line to remove (L=AB) can have up to two triangles (T=ABC)
        T has lines AB, BC, AC; the end result is to remove T and merge BC and AC (as A and B become one point)
          Change T in AC and BC to be the other triangle from the other line
          Remove BC from point C
          Remove BC from the mesh
          Remove T from point C
          Remove T from mesh
        Remove AB from the mesh
        For all triangles in the mesh which include B, make them use A instead
        For all lines in the mesh which include B, make them use A instead
        Remove B from the mesh
        """
        (A,B) = line.get_points()
        (xA,yA) = A.coords()
        (xB,yB) = B.coords()
        xA = (xA+xB)/2.0
        yA = (yA+yB)/2.0
        A.set_coords( coords=(xA,yA) )
        for t in line.triangles:
            if t is None: continue
            C = t.get_other_point( A,B )
            AC = A.find_line_segment_to( C )
            BC = B.find_line_segment_to( C )
            tac = AC.other_triangle(t)
            tbc = BC.other_triangle(t)
            AC.swap_triangle(t,tbc)
            A.remove_from_triangle(t)
            C.remove_from_triangle(t)
            C.remove_from_line(BC)
            self.remove_line(BC)
            self.remove_triangle( t )
            pass
        self.remove_line(line)
        A.remove_from_line(line)
        self.remove_point(B)
        for t in self.triangles:
            if B in t.get_points():
                t.change_vertex(B,A)
                pass
            pass
        for l in self.line_segments:
            if l is line: continue
            lpts = l.get_points()
            if B in lpts:
                print "Change vertex for %s from %s to %s"%(str(l),str(B),str(A))
                l.change_vertex(B,A)
                print "Changed vertex for %s from %s to %s"%(str(l),str(B),str(A))
                pass
            elif A in lpts:
                l.calculate_direction()
                pass
            pass
        for c in self.contours:
            mesh_pts = c["mesh_pts"]
            for i in range(len(mesh_pts)):
                if mesh_pts[i]==B: mesh_pts[i] = A
                pass
            i=0
            while i<len(mesh_pts):
                if (mesh_pts[i]==A) and (mesh_pts[(i+1)%len(mesh_pts)]==A):
                    mesh_pts.pop(i)
                    pass
                else:
                    i+=1
                    pass
                pass
            pass
        pass
    #f remove_small_lines
    def remove_small_lines( self, min_length, verbose=False ):
        """
        Find all lines below a smallest length and remove them by merging the two points

        Return number of lines removed
        Check all lines
        If the line length is not small, continue, else remove the line by merging its two ends
        """
        if verbose:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
            print "Removing small lines"
            self.check_consistent()
            print self.__repr__(verbose=True)
            pass
        lines_removed = 0
        i = 0
        while i<len(self.line_segments):
            l = self.line_segments[i]
            if l.get_length()>min_length:
                i+=1
                continue
            if verbose:
                print "********************************************************************************"
                print "Length %f of %s, want to remove"%(l.get_length(),str(l))
                pass
            #self.check_consistent()
            self.merge_two_line_ends(l, verbose=verbose)
            #print self.__repr__(verbose=True)
            #self.check_consistent()
            lines_removed += 1
            pass
        if verbose:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
            print "Lines removed",lines_removed
            print self.__repr__(verbose=True)
            self.check_consistent()
        return lines_removed
    #f remove_small_area_triangles
    def remove_small_area_triangles( self, min_area, verbose=False ):
        """
        Find all triangles below a smallest area and remove them by moving the mesh point not on the longest side to be in line with the longest side

        Return number of triangles removed
        Check all triangles
        If the triangle area is not small, continue, else remove the triangle
        The triangle to remove (T=ABC) must be 'nearly a straight line', i.e. the longest side of the triangle (AB)
        IF AB is the longest side then C = (qA + (1-q)B) + e.perp(BA) where 0<q<1
        (A-C).(B-C) = ((q-1)(A+B) . 
        Find the point of the triangle that is not on the longest side (C), and move it gently so it is on the longest side
          Note that C = k.AB + l.(normal to AB); find k, then make C=k.AB
          Find the triangle the other side of AB (T2=ABX)
          If T2 exists (i.e. AB borders 2 triangles)
            We want to have 2 triangles ACX and BCX
            Since C is between A and B on AB, ACBX must be convex
            So swap the diagonal (AB.swap_diagonal( mesh ) )
          Else (AB borders one triangle)
            Remove T from lines AC and BC
            Remove T from points A and B
            Remove AB from points A and B
            Remove T from the mesh
            Remove AB from the mesh
        """
        if verbose:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
            print "Removing triangles"
            self.check_consistent()
            print self.__repr__(verbose=True)
            pass
        #verbose = True
        triangles_removed = 0
        i = 0
        while i<len(self.triangles):
            t = self.triangles[i]
            if t.get_area()>min_area:
                i+=1
                continue
            if verbose:
                print "********************************************************************************"
                print "Area %f of %s, want to remove"%(t.get_area(),str(t))
                pass
            (A,B,C) = t.get_points()

            (Ax,Ay) = A.coords()
            (Bx,By) = B.coords()
            (Cx,Cy) = C.coords()
            lAB2 = (Ax-Bx)*(Ax-Bx)+(Ay-By)*(Ay-By)
            lAC2 = (Ax-Cx)*(Ax-Cx)+(Ay-Cy)*(Ay-Cy)
            lBC2 = (Bx-Cx)*(Bx-Cx)+(By-Cy)*(By-Cy)
            while (lAB2<lAC2) or (lAB2<lBC2):
                (A,B,C) = (B,C,A)
                (lAB2, lAC2, lBC2) = (lBC2, lAB2, lAC2)
                pass
            (Ax,Ay) = A.coords()
            (Bx,By) = B.coords()
            (Cx,Cy) = C.coords()
            l = (By-Ay)*(By-Ay)+(Bx-Ax)*(Bx-Ax)
            k = ((Cx-Ax)*(Bx-Ax) + (Cy-Ay)*(By-Ay)) / l
            if verbose:
                print (Ax,Ay), (Bx,By), (Cx,Cy), k, l
                print "Will move C to",(Ax*(1-k)+k*Bx,Ay*(1-k)+k*By)
                pass
            C.set_coords( (Ax*(1-k)+k*Bx,Ay*(1-k)+k*By) )
            for cl in C.all_lines():
                cl.calculate_direction()
                pass
            AB = A.find_line_segment_to(B)
            AC = A.find_line_segment_to(C)
            BC = B.find_line_segment_to(C)
            if AB.num_triangles()==1:
                #self.check_consistent()
                A.remove_from_triangle( t )
                B.remove_from_triangle( t )
                AC.remove_from_triangle( t )
                BC.remove_from_triangle( t )
                A.remove_from_line( AB )
                B.remove_from_line( AB )
                self.remove_line( AB )
                self.remove_triangle( t )
                #self.check_consistent()
                pass
            else:
                #self.check_consistent()
                AB.swap_diagonal( self, verbose=verbose )
                #self.check_consistent()
                pass
            i+=1
            pass
        if verbose:
            print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
            print "Triangles removed",triangles_removed
            print self.__repr__(verbose=True)
            self.check_consistent()
        return triangles_removed
    #f find_large_area_triangle_centroids
    def find_large_area_triangle_centroids( self, max_area, verbose=False ):
        """
        Find all triangles above a max area and return a list of their centroids
        """
        centroids = []
        for t in self.triangles:
            area = t.get_area()
            if area<max_area:
                i+=1
                continue
            if verbose:
                print "*"*80
                print "Area %f of %s, want to break"%(t.get_area(),str(t))
                pass
            (A,B,C) = t.get_points()

            (Ax,Ay) = A.coords()
            (Bx,By) = B.coords()
            (Cx,Cy) = C.coords()
            (Dx, Dy) = ((Ax+Bx+Cx)/3, (Ay+By+Cy)/3)
            centroids.append( (area,Dx,Dy) )
        return centroids
    #f shorten_quad_diagonals
    def shorten_quad_diagonals( self, verbose=False ):
        """
        Swap diagonals of quadrilaterals by picking the shortest of each one

        Return number of triangles changed
        """
        work_done = 0
        for i in range(len(self.line_segments)):
            l = self.line_segments[i]
            if l.can_shorten_diagonal():
                l.swap_diagonal( self, verbose=verbose  )
                work_done += 1
                #self.check_consistent()
                #return
                pass
            pass
        #self.split_line_segment_in_half( self.line_segments[0] )

        return work_done
    #f find_line_segments_on_line
    def find_line_segments_on_line( self, pt0, pt1 ):
        """
        Find all the line segments that intersect a line between two mesh points if the line is not between two mesh points already
        """
        intersections = []
        (x0,y0) = pt0.coords()
        (x1,y1) = pt1.coords()
        (dx,dy) = (x1-x0, y1-y0)
        for l in self.line_segments:
            l_pts = l.get_points()
            if pt0 in l_pts or pt1 in l_pts: continue
            intersect = l.find_intersection( (x0,y0), (dx,dy) )
            if intersect is not None:
                intersections.append( (l, intersect) )
                pass
            pass
        return intersections
    #f split_a_line_for_contour_segment
    def split_a_line_for_contour_segment( self, mesh_pts, pt_num, intersections, max_breaks=1 ):
        """
        Split a line between mesh_pts[pt_num-1] and mesh_pts[pt_num] using one or more of the list of intersections.

        intersections is returned by find_line_segments_on_line, and so is a list of (line segment, intersection info) tuples
        intersection info is (k, l, x, y) where k is how far along the line segment to split and l is how far between the two mesh points the split would be.
        (k and l are in the range 0 to 1).

        First cut is to split at lowest l...
        """
        min_l = 2
        min_i = None
        for i in range(len(intersections)):
            if intersections[i][1][1]<min_l:
                min_l=intersections[i][1][1]
                min_i=i
                pass
            pass
        (x,y) = (intersections[min_i][1][2],intersections[min_i][1][3])
        pt = self.add_point(bezier.c_point( coords=(x,y) ), append_to_numbers=True)
        mesh_pts.insert( pt_num, pt )
        self.split_line_segment( intersections[min_i][0], pt )
        return
    #f ensure_contours_on_mesh
    def ensure_contours_on_mesh( self, verbose=False ):
        """
        For every line segment in every contour, if the line segment is not present in the mesh, split

        Check every pair of mesh points on every contour.
        If the line between the points is not a line segment, then chose one from the set of line segments that lies between the two points and split that at the correct point, adding this new point to the contour
        Return the total number of lines split.

        At each call the situation is improved; however, the method should be invoked repeatedly until it returns zero if all contours are required to be on the mesh.
        Between calls of this method the mesh can be optimized with, for example, calls of the shorten_quad_diagonals() method.
        """
        lines_changed = 0
        for c in self.contours:
            mesh_pts = c["mesh_pts"]
            p0 = mesh_pts[-1]
            if not c["closed"]: p0=None
            i = 0
            while i<len(mesh_pts):
                p = mesh_pts[i]
                if (p0 is not None) and (p0.find_line_segment_to(p) is None):
                    #verbose = (p0.entry_number==7) and (p.entry_number==11)
                    l0 = p0.find_line_segment_toward( p, verbose=verbose )
                    l1 = p.find_line_segment_toward( p0, verbose=verbose )
                    if verbose:
                        print i, mesh_pts, p0, p, l0, l1
                    if l0 is not None:
                        mesh_pts.insert( i, l0.other_end(p0) )
                        lines_changed+=1
                        pass
                    elif l1 is not None:
                        mesh_pts.insert( i, l1.other_end(p) )
                        lines_changed+=1
                        p=p0
                        i-=1
                        pass
                    else:
                        ls = self.find_line_segments_on_line( p0, p )
                        if len(ls)>0:
                            self.split_a_line_for_contour_segment( mesh_pts, i, ls )
                            lines_changed+=1
                            pass
                        pass
                    pass
                p0 = p
                i+=1
                pass
            pass
        return lines_changed

    #f assign_winding_order_to_contours
    def assign_winding_order_to_contours( self ):
        """
        Assign a winding order to mesh line segments which make up lines

        For every line segment making a contour set the winding order to be that of the contour
        """
        for l in self.line_segments:
            l.set_winding_order()
            pass

        for c in self.contours:
            mesh_pts = c["mesh_pts"]
            p0 = mesh_pts[-1]
            if not c["closed"]: p0=None
            i = 0
            while i<len(mesh_pts):
                p = mesh_pts[i]
                if p0 is not None:
                    l = p0.find_line_segment_to(p)
                    if l is not None: # Should be none if ensure_contours_on_mesh process is complete, but do not require it
                        l.set_winding_order(p0,p)
                        pass
                    pass
                p0 = p
                i+=1
                pass
            pass
        pass
    #f assign_winding_order_to_mesh
    def assign_winding_order_to_mesh( self ):
        """
        Assign a winding order to the whole mesh given that some lines have a winding order

        Clear the winding order for each triangle
        Work from the 'outside' in of the mesh.
           Create a list of lines segments that border exactly one triangle
           Pop the first line in the list
             If there are two triangles for the line and both already have a winding order, move on
             If there is one triangle for the line, set its winding order based on the winding order of the line
             If there are two triangles for the line then at least one has a winding order, and set the winding order of the other triangle
             For each triangle whose winding order is set, and the other lines in the triangle to the list of line segments to work on
           Repeat until the work list is empty

        If a line has 'None' as its winding order, then the triangles on both sides have the same winding order
        If a line has 'False' as its winding order, then the triangle on the 'left' of the line (that whose third point is left of line.pts[0] -> line.pts[1]) has -1 of the other triangle
        If a line has 'True' as its winding order, then the triangle on the 'left' of the line (that whose third point is left of line.pts[0] -> line.pts[1]) has +1 of the other triangle
        """
        for t in self.triangles:
            t.set_winding_order()
            pass

        work_list = []
        for l in self.line_segments:
            if l.num_triangles()==1:
                work_list.append(l)
                pass
            pass

        while len(work_list)>0:
            l = work_list.pop(0)
            (t0, t1) = l.triangles
            if (t1 is not None) and (t0.winding_order is not None) and (t1.winding_order is not None): continue
            if t1 is None:
                work_list.extend( t0.set_winding_order(l) )
                pass
            elif (t1 is not None) and (t1.winding_order is not None):
                work_list.extend( t0.set_winding_order(l,t1.winding_order) )
                pass
            elif (t0 is not None) and (t0.winding_order is not None):
                work_list.extend( t1.set_winding_order(l,t0.winding_order) )
                pass
            pass
        pass
    #f get_vertices_and_triangles
    def get_vertices_and_triangles( self ):
        """
        Generate lists of all vertices

        Generate triangle strips (list of triangle strips) for all triangles
          triangle strip = [ vertex index, vertex index, ...]
        Generate list of edges in counter-clockwise order
          an edge is a line that does not have two triangles
          may need to have a 'other-handedness flag' so the mesh can be 'inverted'
        """
        pass
    #f __repr__
    def __repr__( self, verbose=False ):
        result =  "mesh\n"
        result += "  pts: %s\n"%(str(self.point_set))
        result += "  lns: %s\n"%(str(self.line_segments))
        result += "  tris: %s\n"%(str(self.triangles))
        if verbose:
            result = result.replace(", ",",\n        ")
        return result

#a Mesh pygame test
def mesh_test_get_draw_fn():
    import gjslib.graphics.font as font
    x = c_mesh()
    bezier_list = []
    for (p0,c0,p1) in [ ( (100,100), (110,90), (150, 50) ),
                        ( (150, 50), (150,150), (200,100) ),
                        ( (200,100), (150,150), (200,200) ),
                        ( (200,200), (175,175), (150,150) ),
                        ( (150,150), (125,125), (100,100) ),
                    ]:
        p0 = bezier.c_point(coords=p0)
        c0 = bezier.c_point(coords=c0)
        p1 = bezier.c_point(coords=p1)
        bezier_list.append(bezier.c_bezier2(pts=(p0,c0,p1)))
        pass
    f = font.c_font("benegraphic").load_from_ttx("/Users/gstark_old/a.ttx")
    bezier_list = f.create_bezier_lists( "ampersand" )[0]
    print bezier_list
    x.from_bezier_list( bezier_list=bezier_list, straightness=10 )
    #print x
    print "Normalizing"
    x.normalize()
    print x.__repr__(verbose=True)
    print "Filling"
    x.fill_convex_hull_with_triangles()
    print x.__repr__(verbose=True)
    x.check_consistent()
    print "Shorten"
    i=0
    while x.shorten_quad_diagonals(i>100)>0:
        i+=1
        if i>101:break
        pass
    print "broken out after",i
    x.check_consistent()
    def draw_fn( screen ):
        draw_triangles( screen, x.triangles )
        draw_contour( screen, x )
    return draw_fn

 *)
  (*f All done *)
end
