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
 * @file          test_mesh.ml
 * @brief         Test suite for the mesh modules
 *
 *)

(*a Documentation
 * 
 * This is a test suite for the ATCF mesh library
 *
 * Features that require testing:
 *
 * Point
 *
 * create
 * check_consistent
 * coords returns the coords (test_suite_point_create.create.xy_0_0)
 * set_coord sets the coords (test_suite_point_create.create.xy_4_5)
 * add_to_line updates the lines the point belongs to (test_suite_line_point.add_rem_pt)
 * remove_from_line updates the lines the point belongs to (test_suite_line_point.add_rem_pt)
 * used_in_lines (test_suite_point_create.create.xy_4_5)
 * all_lines is correct length (test_suite_line_points.points)
 * all_lines has correct lines (?)
 * is_on_line (test_suite_line_point.add_rem_pt)
 * find_line_segment_to     finds correct line (test_suite_line_points.points)
 * find_line_segment_to     finds no line (test_suite_line_points.points)
 * find_line_segment_to_exn finds correct line (test_suite_line_points.points)
 * find_line_segment_to_exn raises exception (test_suite_line_points.points)
 * compare_with
 * add_to_triangle
 * remove_from_triangle
 *
 * Line
 *
 * create
 * check_consistent
 * get_points
 * get_length
 * other_end
 * is_parallel_to
 * num_triangles
 * add_to_triangle
 * remove_from_triangle
 * swap_triangle
 * other_triangle
 * is_part_of_triangle
 * merge_to_new
 * side_of_line_2d
 * set_winding_order
 * get_winding_order
 * change_vertex
 *
 * Triangle
 *)

(*a Libraries used
 *
 * OUnit is required as the test framework
 *
 *)
open Atcflib
open Mesh
open OUnit

(*a Helper functions *)
(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found

(*f sfmt <string format> <arg> * -> string
 *    formats a string with arguments
 *)
let sfmt = Printf.sprintf

(*f add_rect *)
let add_rect mesh x0 y0 x1 y1 = 
  let (p0, p1, p2, p3) = (
      Mesh.add_point_from_vector mesh (Vector.make2 x0 y0),
      Mesh.add_point_from_vector mesh (Vector.make2 x1 y0),
      Mesh.add_point_from_vector mesh (Vector.make2 x1 y1),
      Mesh.add_point_from_vector mesh (Vector.make2 x0 y1))
    in
 Mesh.add_triangle_from_points mesh p1 p0 p2;
 Mesh.add_triangle_from_points mesh p0 p2 p3

(*a Assertion functions, to simplify OUnit test code *)
(*f assert_equal_float : string -> float -> float -> unit
 *
 * Assert that the two floats are equal (or near enough),
 * with a message msg and the two float values
 *
 *)
let assert_equal_float msg v0 v1 =
  let diff = abs_float ( v0 -. v1 ) in
  let close_enough = (diff < epsilon) in
  assert_bool (sfmt "%s:%f:%f" msg v0 v1) close_enough

(*f assert_equal_int : string -> int -> int -> unit
 *
 * Assert that the two ints are equal with a message msg and the two
 * float values
 * 
 *)
let assert_equal_int msg v0 v1 =
  let close_enough = (v0 = v1) in
  assert_bool (sfmt "%s:%d:%d" msg v0 v1) close_enough

(*f assert_equal_quat : string -> quaternion -> float array -> unit
 *
 * Assert that a quaternion has certain r,i,j,k (or near enough),
 * with a message msg
 *
 *)
let assert_equal_quat msg q rijk =
    let qrijk = Quaternion.get_rijk q in
    assert_equal_float (sfmt "%s:r:" msg) qrijk.(0) rijk.(0) ;
    assert_equal_float (sfmt "%s:i:" msg) qrijk.(1) rijk.(1) ;
    assert_equal_float (sfmt "%s:j:" msg) qrijk.(2) rijk.(2) ;
    assert_equal_float (sfmt "%s:k:" msg) qrijk.(3) rijk.(3)

(*f assert_coords : vector -> float array -> unit
 *
 * Assert that coordinates of the vector are close enough to the float array
 * 
 *)
let assert_coords v cs =
  let assert_coord = fun i a -> assert_equal_float (sfmt "Coord %d" i) a cs.(i) in
  Array.iteri assert_coord (Vector.coords v)

(*f assert_vector : vector -> vector -> unit
 *
 * Assert that coordinates of the vector are close enough to the other vector
 * 
 *)
let assert_vector v cs =
  assert_coords v (Vector.coords cs)

(*a Point test suite *)
(*b Point creation tests *)
let test_suite_point_create = 
  "create" >::: [
      ("xy_0_0" >::
         fun ctxt ->
         let pt = Point.create (Vector.make2 0. 0.) in
         let cs = Point.coords pt in
         Printf.printf "%s\n" (Point.str pt);
         assert_equal_float "x" cs.(0) 0. ;
         assert_equal_float "y" cs.(1) 0. ;
         assert_bool "Not used in line" (not (Point.used_in_lines pt))

      ) ;
      ("xy_2_3" >::
         fun ctxt ->
         let pt = Point.create (Vector.make2 2. 3.) in
         let cs = Point.coords pt in
         Printf.printf "%s\n" (Point.str pt);
         assert_equal_float "x" cs.(0) 2. ;
         assert_equal_float "y" cs.(1) 3. ;
         assert_bool "Not used in line" (not (Point.used_in_lines pt))
      ) ;
      ("xy_4_5" >::
         fun ctxt ->
         let pt = Point.create (Vector.make2 2. 3.) in
         Point.set_coord pt 0 2.;
         Point.set_coord pt 1 3.;
         let cs = Point.coords pt in
         Printf.printf "%s\n" (Point.str pt);
         assert_equal_float "x" cs.(0) 2. ;
         assert_equal_float "y" cs.(1) 3. ;
         assert_equal_int "P0 is on no lines " (List.length (Point.all_lines pt)) 0 ;
         assert_bool "Not used in line" (not (Point.used_in_lines pt))
      ) ;
    ]

(*b Point test suite - combine individual suites *)
let test_suite_point =
  "Test points" >:::
    [
      test_suite_point_create ;
    ]

(*a Line test suite *)
(*b Line creation tests *)
let test_suite_line_create = 
  "create" >::: [
      ("0_1_3_6" >::
         fun ctxt ->
         let p0 = Point.create (Vector.make2 0. 1.) in
         let p1 = Point.create (Vector.make2 3. 6.) in
         let p2 = Point.create (Vector.make2 4. 5.) in
         let l = Line.create p0 p1 in
         Line.check_consistent l;
         Point.check_consistent p0;
         Point.check_consistent p1;
         Point.check_consistent p2;
         ()
      ) ;
    ]

(*b Line creation tests *)
let test_suite_line_points = 
  "points" >::: [
      ("0_1_3_6" >::
         fun ctxt ->
         let p0 = Point.create (Vector.make2 0. 1.) in
         let p1 = Point.create (Vector.make2 3. 6.) in
         let p2 = Point.create (Vector.make2 4. 5.) in
         let l = Line.create p0 p1 in
         assert_bool "P0 used in line" (Point.used_in_lines p0);
         assert_bool "P1 used in line" (Point.used_in_lines p1);
         assert_bool "P0 is on line l" (Point.is_on_line p0 l);
         assert_bool "P1 is on line l" (Point.is_on_line p1 l);
         assert_equal_int "P0 is on one line " (List.length (Point.all_lines p0)) 1 ;
         assert_equal_int "P1 is on one line " (List.length (Point.all_lines p1)) 1 ;
         assert_bool "P0 to P1 is line l" ((option_get (Point.find_line_segment_to p0 p1))==l);
         assert_bool "P1 to P0 is line l" ((option_get (Point.find_line_segment_to p1 p0))==l);
         assert_bool "P1 to P2 is None"   ((option_is_none (Point.find_line_segment_to p1 p2)));
         assert_bool "P0 to P1 is line l" ((Point.find_line_segment_to_exn p0 p1)==l);
         assert_bool "P1 to P0 is line l" ((Point.find_line_segment_to_exn p1 p0)==l);
         assert_bool "P1 to P2 raises correct exception"
                     (try (ignore (Point.find_line_segment_to_exn p1 p2); false)
                      with Line_not_found _ -> true);
         assert_bool "P0 toward P1 vector is line l" ((option_get (Point.find_line_segment_toward p0 (Vector.make2 3. 6.)))==l);
         assert_bool "P1 toward P0 vector is line l" ((option_get (Point.find_line_segment_toward p1 (Vector.make2 0. 1.)))==l);
         assert_bool "P0 toward 2P1+P0 vector is line l" ((option_get (Point.find_line_segment_toward p0 (Vector.make2 6. 11.)))==l);
         assert_bool "P0 toward 2P1 vector is None" ((option_is_none (Point.find_line_segment_toward p0 (Vector.make2 6. 10.))));

         ()

      ) ;
      ("add_rem_pt" >::
         fun ctxt ->
         let p0 = Point.create (Vector.make2 0. 1.) in
         let p1 = Point.create (Vector.make2 3. 6.) in
         let p2 = Point.create (Vector.make2 4. 5.) in
         let l = Line.create p0 p1 in
         assert_bool "1. P0 used in line" (Point.used_in_lines p0);
         assert_bool "1. P1 used in line" (Point.used_in_lines p1);
         assert_bool "1. P0 is on line l" (Point.is_on_line p0 l);
         assert_bool "1. P1 is on line l" (Point.is_on_line p1 l);
         Point.remove_from_line p0 l;
         assert_bool "2. P0 not used in line" (not (Point.used_in_lines p0));
         assert_bool "2. P1 used in line" (Point.used_in_lines p1);
         assert_bool "2. P0 is not on line l" (not (Point.is_on_line p0 l));
         assert_bool "2. P1 is on line l" (Point.is_on_line p1 l);
         Point.remove_from_line p1 l;
         assert_bool "3. P0 not used in line" (not (Point.used_in_lines p0));
         assert_bool "3. P1 not used in line" (not (Point.used_in_lines p1));
         assert_bool "3. P0 is not on line l" (not (Point.is_on_line p0 l));
         assert_bool "3. P1 is not on line l" (not (Point.is_on_line p1 l));
         Point.add_to_line p1 l p0 0;
         assert_bool "4. P0 not used in line" (not (Point.used_in_lines p0));
         assert_bool "4. P1 used in line" (Point.used_in_lines p1);
         assert_bool "4. P0 is not on line l" (not (Point.is_on_line p0 l));
         assert_bool "4. P1 is on line l" (Point.is_on_line p1 l);
         Point.add_to_line p0 l p1 1;
         assert_bool "5. P0 used in line" (Point.used_in_lines p0);
         assert_bool "5. P1 used in line" (Point.used_in_lines p1);
         assert_bool "5. P0 is on line l" (Point.is_on_line p0 l);
         assert_bool "5. P1 is on line l" (Point.is_on_line p1 l);
         Line.check_consistent l;
         assert_bool "p0 inconsistent correctly" 
                     (try (Point.check_consistent p0; false) with Not_in_line _ -> true);
         assert_bool "p1 inconsistent correctly" 
                     (try (Point.check_consistent p1; false) with Not_in_line _ -> true);
         Point.check_consistent p2;
         ()
      );
    ]

(*b Line test suite - combine individual suites *)
let test_suite_line =
  "Test lines" >:::
    [
      test_suite_line_create ;
      test_suite_line_points ;
    ]

(*a Mesh test suite *)
(*b Mesh creation tests *)
let test_suite_mesh_create = 
    "create" >::: [
        ("module" >::
           fun ctxt ->
           let mesh = Mesh.create () in
           add_rect mesh 0. 0. 1. 1.;
           Mesh.check_consistent mesh;
        ) ;
    ]

(*b Mesh test suite - combine individual suites *)
let test_suite_mesh =
  "Test meshs" >:::
    [
      test_suite_mesh_create ;
    ]

(*a All test suites, toplevel *)
let test_suites = "All tests" >::: [
     test_suite_point ;
     test_suite_line ;
     test_suite_mesh ;
    ]


let _ = 
    at_exit Gc.full_major ;
    run_test_tt_main  test_suites ;
