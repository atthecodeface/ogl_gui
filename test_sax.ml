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
 * This is a test suite for the ATCF XML library
 *
 * Features that require testing:
 *
 * Unicode
 *
 * reading of characters 0 to 127
 * reading of characters 0x80 to 0x7ff 
 * reading of characters 0x800 to 0xffff
 * reading of characters 0x10000 to 0x10ffff
 *)

(*a Libraries used
 *
 * OUnit is required as the test framework
 *
 *)
open Sax
open OUnit
let epsilon = 1.E-10

(*a Helper functions *)
(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found

(*f sfmt <string format> <arg> * -> string
 *    formats a string with arguments
 *)
let sfmt = Printf.sprintf

(*f stream_of_string *)
let stream_of_string test_str =
  let opt_char s n = if (n<(String.length s)) then (Some s.[n]) else None in
  Stream.from (opt_char test_str)

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

(*a Unicode test suite *)
(*f utf8_test_ascii *)
let utf8_test_ascii test_str =
  let f = stream_of_string test_str in
  for i = 0 to ((String.length test_str)-1) do
    let pt = Utf8.getc f in
    let (s,c) = Utf8.sc pt in
    assert_equal_int "c" c (int_of_char (test_str.[i]))
  done

(*b Single byte UTF8 tests *)
let test_suite_unicode_utf8_one_byte =
  "one_byte" >::: [
      ("newline" >::
         fun ctxt ->
         let pt = Utf8.newline in
         let (s,c) = Utf8.sc pt in
         assert_equal_int "c" c 10
      ) ;
      ("a_to_z" >::
         fun ctxt ->
         utf8_test_ascii "abcdefghijklmnopqrstuvwxyz"
      ) ;
      ("A_TO_Z" >::
         fun ctxt ->
         utf8_test_ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      ) ;
      ("0_to_9" >::
         fun ctxt ->
         utf8_test_ascii "0123456789"
      ) ;
      ("symbols" >::
         fun ctxt ->
         utf8_test_ascii " !@#$%^&*()[]-=_+{}`~,.<>?/\\\"|" (* ±§ are >127*)
      ) ;
    ]

(*b Two-byte UTF8 tests *)
let test_suite_unicode_utf8_two_bytes =
  "two_bytes" >::: [
      ("0x80" >::
         fun ctxt ->
         let test_str = "\xc2\x80" in
         let opt_char s n = if (n<(String.length s)) then (Some s.[n]) else None in
         let f = Stream.from (opt_char test_str) in
         let pt = Utf8.getc f in
         let (s,c) = Utf8.sc pt in
         assert_equal_int "c" c 0x80 ;
         assert_equal_int "ls" (String.length s) 2
      ) ;
    ]

(*b Three-byte UTF8 tests *)
let test_suite_unicode_utf8_three_bytes =
  "three_bytes" >::: [
      ("0x800" >::
         fun ctxt ->
         let test_str = "\xe0\xa0\x80" in
         let opt_char s n = if (n<(String.length s)) then (Some s.[n]) else None in
         let f = Stream.from (opt_char test_str) in
         let pt = Utf8.getc f in
         let (s,c) = Utf8.sc pt in
         assert_equal_int "c" c 0x800 ;
         assert_equal_int "ls" (String.length s) 3
      ) ;
    ]

(*b Four-byte UTF8 tests *)
let test_suite_unicode_utf8_four_bytes =
  "four_bytes" >::: [
      ("0x10000" >::
         fun ctxt ->
         let test_str = "\xf0\x90\x80\x80" in
         let opt_char s n = if (n<(String.length s)) then (Some s.[n]) else None in
         let f = Stream.from (opt_char test_str) in
         let pt = Utf8.getc f in
         let (s,c) = Utf8.sc pt in
         assert_equal_int "c" c 0x10000 ;
         assert_equal_int "ls" (String.length s) 4
      ) ;
    ]

(*b Unicode test suite - combine individual suites *)
let test_suite_unicode =
  "Test unicodes" >:::
    [
      test_suite_unicode_utf8_one_byte ;
      test_suite_unicode_utf8_two_bytes ;
      test_suite_unicode_utf8_three_bytes ;
      test_suite_unicode_utf8_four_bytes ;
    ]

(*a String test suite *)
(*f test_match_string *)
let null_content_handler = Content_handler.create 0 ()
let test_match_string stream_string str_exp_list =
  ((sfmt "%s" stream_string) >::
     fun ctxt ->
     let xmls = XMLReader.create ~content_handler:null_content_handler () in
     XMLReader.set_stream xmls (stream_of_string stream_string);
     let check se = 
       let (s,e) = se in
       assert_bool (sfmt "'%s' contains '%s'" stream_string s) ((XMLReader.match_string xmls s)=e)
     in
     List.iter check str_exp_list
  )

(*f test_build_string *)
let test_build_string stream_string filter string =
  ( (sfmt "%s build" stream_string) >::
      fun ctxt ->
      let xmls = XMLReader.create ~content_handler:null_content_handler () in
      XMLReader.set_stream xmls (stream_of_string stream_string);
      let s = (XMLReader.build_string xmls "" filter) in
      assert_bool (sfmt "'%s' first name is '%s'" stream_string string) (string = s)
  )

(*b Single byte UTF8 tests *)
let test_suite_string_parse =
  "parse" >::: [
      (test_match_string "<?xml fred" [("<?xml",true);
                                       ("<?xml",false);
                                       (" fr",true);
                                       (" fr",false);
                                       ("edb",false);
                                       ("ed",true);]);
      (test_match_string "This is a story about a train" [("Coach",false);
                                       ("banana",false);
                                       ("This is a story",true);
                                       ("about",false);
                                       ("a train",false);
                                       (" about",true);]);
      (test_build_string "Stuff with stuff" (fun x -> (x!=eof)) "Stuff with stuff");
      (test_build_string "Stuff with stuff" XMLReader.is_name_start_char "Stuff");
      (test_build_string "Stuff with stuff" (fun x -> ((x>=65) && (x<=90))) "S");
      (test_build_string " \x09\x0aStuff" XMLReader.is_whitespace " \x09\x0a");
      (test_build_string " \x09\x0d\x0aStuff" XMLReader.is_whitespace " \x09\x0a");
    ]

(*b String test suite - combine individual suites *)
let test_suite_string =
  "Test strings" >:::
    [
      test_suite_string_parse ;
    ]

(*a XML test suite *)
(*b Xml test suite *)
let print_string_pair nv =
  let (n,v)=nv in
  Printf.printf "%s=%s " n v
let comment state comment = 
    Printf.printf "Comment %s\n%!" comment;
    ()
let processing_instruction state ~target ~data = 
    Printf.printf "Processing instruction %s : %s\n%!" target data;
    ()
let start_document state ~rootName = 
    Printf.printf "Document start %s\n%!" rootName;
    ()
let end_document state = 
    Printf.printf "End document\n%!";
    ()
let start_element state ~uri ~localName ~qName ~atts= 
    Printf.printf "Start element %s:attrs %a\n%!" qName (fun c nvs->List.iter print_string_pair nvs) atts;
    ()
let end_element state ~uri ~localName ~qName = 
    Printf.printf "End element %s\n%!" qName;
    ()
let test_content_handler =
  Content_handler.create
    ~start_document_callback:start_document
    ~end_document_callback:end_document
    ~start_element_callback:start_element
    ~end_element_callback:end_element
    ~pi_callback:processing_instruction
    ~callback_state:0
  ()

let test_suite_xml_parse =
  "parse" >::: [
  "blah" >::
     fun ctxt ->
     let xmls = XMLReader.create ~comment_callback:comment
                                      ~content_handler:test_content_handler
                                      () in
     XMLReader.parse_string xmls "<?xml?><fred banana='yes'>asd<!--this is my comment--><?pi?></fred>";
     ()
    ]

(*b Xml test suite - combine individual suites *)
let test_suite_xml =
  "Test xmls" >:::
    [
      test_suite_xml_parse ;
    ]

(*a All test suites, toplevel *)
let test_suites = "All tests" >::: [
     test_suite_unicode ;
     test_suite_string ;
     test_suite_xml ;
    ]


let _ = 
    at_exit Gc.full_major ;
    run_test_tt_main  test_suites ;
