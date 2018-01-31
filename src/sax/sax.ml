let eof = -1
(*f sfmt *)
let sfmt = Printf.sprintf

(*f trace *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

(*f option_is_some/is_none/get *)
let option_is_none o = match o with None -> true | Some _ -> false
let option_is_some o = match o with None -> false | Some _ -> true
let option_get     x = match x with Some p -> p | None -> raise Not_found

(*f >>= standard monadic function *)
let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

(*m Utf8
 *)
module Utf8 = struct
  (*t Type *)
  type t = {
    c : int;
    s : string;
    } 

  (*f Exceptions *)
  exception Bad_unicode

  (*v newline *)
  let newline = {c=0xa; s="\n"}

  (*f _getc *)
  let _getc (f:char Stream.t) =
    try 
      let c = Stream.next f in
      ((String.make 1 c), (int_of_char c))
    with Stream.Failure -> ("", eof)

  (*f getc - get unicode character from stream *)
  let getc f =
    let rec extended_unicode nbytes acc_s acc_c =
      if (nbytes==0) then {s=acc_s; c=acc_c}
      else
        let (s,c) = _getc f in
        if ((c>=0x80) && (c<=0xbf)) then
          let nacc_c = ((acc_c lsl 6) lor (c land 0x3f)) in
          let nacc_s = acc_s ^ s in
          (extended_unicode (nbytes-1) nacc_s nacc_c)
        else
          raise Bad_unicode
    in
    let (s,c) = _getc f in
    if (c == eof) then {s;c}
    else if (c<0x80) then {s;c}
    else if ((c>=0xc0) && (c<=0xdf)) then 
      (extended_unicode 1 s (c land 0x1f))
    else if ((c>=0xe0) && (c<=0xef)) then 
      (extended_unicode 2 s (c land 0x0f))
    else if ((c>=0xf0) && (c<=0xf7)) then 
      (extended_unicode 3 s (c land 0x07))
    else
      raise Bad_unicode

  (*f sc *)
  let sc t = (t.s, t.c)

  (*f All done *)
end

(*m Attributes *)
module Attributes = struct 
  type t = {
    qName : string;
    uri : string;
    localName : string;
    value : string;
    attr_type : string;
    }

  let create ~qName ~uri ~localName ~value ?attr_type:(attr_type="CDATA") _ =
    {
    qName; uri; localName; value; attr_type
    }

  let nameValues attlist = 
    let name_value acc att = (att.qName,att.value)::acc in
    List.fold_left name_value [] attlist

  let getValue ~default attlist name =
    let find_name acc att =
      if (att.qName=name) then att.value else acc
    in
    List.fold_left find_name default attlist
end

(*m Callback types *)
type attribute = Attributes.t
type 'a t_callback = 'a -> string -> unit
type 'a t_start_document_callback = 'a -> rootName:string -> unit
type 'a t_end_document_callback = 'a -> unit
type 'a t_start_element_callback = 'a -> uri:string -> localName:string -> qName:string -> atts:attribute list -> unit
type 'a t_end_element_callback = 'a -> uri:string -> localName:string -> qName:string -> unit
type 'a t_start_prefix_mapping_callback = 'a -> prefix:string -> prefix:string -> unit
type 'a t_end_prefix_mapping_callback = 'a -> prefix:string -> unit
type 'a t_processing_instruction_callback = 'a -> target:string -> data:string -> unit
type 'a t_skipped_entity_callback = 'a -> name:string -> unit
type 'a t_characters_callback = 'a -> string -> start:int -> length:int -> unit
type 'a t_ignorable_whitespace_callback = 'a -> string -> start:int -> length:int -> unit

(*m Content_handler *)
module Content_handler : sig
  (*t type *)
  type 'a t = {
  start_document_callback : 'a t_start_document_callback option;
  end_document_callback   : 'a t_end_document_callback option;
  start_element_callback  : 'a t_start_element_callback option;
  end_element_callback    : 'a t_end_element_callback option;
  start_prefix_mapping_callback  : 'a t_start_prefix_mapping_callback option;
  end_prefix_mapping_callback    : 'a t_end_prefix_mapping_callback option;
  characters_callback             : 'a t_characters_callback option;
  pi_callback             : 'a t_processing_instruction_callback option;
  callback_state          : 'a
    }
  val create : ?start_document_callback : 'a t_start_document_callback ->
             ?end_document_callback : 'a t_end_document_callback ->
             ?start_element_callback : 'a t_start_element_callback ->
             ?end_element_callback : 'a t_end_element_callback ->
             ?start_prefix_mapping_callback : 'a t_start_prefix_mapping_callback ->
             ?end_prefix_mapping_callback : 'a t_end_prefix_mapping_callback ->
             ?characters_callback : 'a t_characters_callback ->
             ?pi_callback : 'a t_processing_instruction_callback ->
             callback_state : 'a -> unit -> 'a t
  val invoke : 'a t -> 'b option -> ('b -> 'a -> 'c) -> unit
  end = struct

  (*t type *)
  type 'a t = {
      start_document_callback : 'a t_start_document_callback option;
      end_document_callback   : 'a t_end_document_callback option;
      start_element_callback  : 'a t_start_element_callback option;
      end_element_callback    : 'a t_end_element_callback option;
      start_prefix_mapping_callback  : 'a t_start_prefix_mapping_callback option;
      end_prefix_mapping_callback    : 'a t_end_prefix_mapping_callback option;
      characters_callback             : 'a t_characters_callback option;
      pi_callback             : 'a t_processing_instruction_callback option;
      callback_state          : 'a
    }
  let create ?start_document_callback
             ?end_document_callback
             ?start_element_callback
             ?end_element_callback
             ?start_prefix_mapping_callback
             ?end_prefix_mapping_callback
             ?characters_callback
             ?pi_callback
             ~callback_state
             () (* required as all others are labelled and some are optional *)
    = {
      start_document_callback;
      end_document_callback;
      start_element_callback;
      end_element_callback;
      start_prefix_mapping_callback;
      end_prefix_mapping_callback;
      characters_callback;
      pi_callback;
      callback_state;
    }
  let invoke t opt_callback inv =
    if (option_is_some opt_callback) then
      let callback = option_get opt_callback in
      inv callback t.callback_state;
      ()
end

(*m XMLReader
 *)
module XMLReader : sig

  (*t type *)
  type 'a t = {
    mutable f            : char Stream.t;
    mutable column       : int;
    mutable line         : int;
    mutable unget_buffer : Utf8.t list;
    mutable bad_unicode  :  bool;
    mutable unget_unicode : Utf8.t option;
    prolog_callback   : 'a t_callback option;
    comment_callback  : 'a t_callback option;
    content_handler   : 'a Content_handler.t;
   }
    val create : ?prolog_callback : 'a t_callback ->
               ?comment_callback : 'a t_callback ->
               content_handler : 'a Content_handler.t  ->
               unit -> 'a t

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

end =  struct

  (*t type *)
  type 'a t = {
    mutable f            : char Stream.t;
    mutable column       : int;
    mutable line         : int;
    mutable unget_buffer : Utf8.t list;
    mutable bad_unicode  :  bool;
    mutable unget_unicode : Utf8.t option;
    prolog_callback   : 'a t_callback option;
    comment_callback  : 'a t_callback option;
    content_handler   : 'a Content_handler.t;
   }

    exception Malformed_xml of string

  (*f set_stream *)
  let set_stream t s =
    t.f <- s;
    t.column <- 0;
    t.line <- 1;
    ()

  (*f create *)
    let create ?prolog_callback
               ?comment_callback
               ~content_handler
               ()
               = 
      { f = Stream.of_string "";
        prolog_callback;
        comment_callback;
        content_handler;
        unget_buffer = [];
        bad_unicode = false;
        unget_unicode = None;
        column = 0;
        line = 1;
    }

  (*f ch_invoke *)
  let ch_invoke t opt_callback inv =
    Content_handler.invoke (t.content_handler) opt_callback inv;
    ()

  (*f malformed_xml - raise exception including location of error *)
  let malformed_xml i s =
    raise (Malformed_xml (sfmt "line %d col %d:%s" i.line i.column s))

  (*f malformed_xml_error - return Error (string including location of error) *)
  let malformed_xml_error i s =
    Error (sfmt "line %d col %d:%s" i.line i.column s)

  (*f is_whitespace c - definition of whitespace [3] Sect 2.3 *)
  let is_whitespace c = ((c==32) || (c==9) || (c==10) || (c==13))

  (*f is_name_start_char c - definition of Name, [4] Sect 2.3
  ":" | [A-Z] | "_" | [a-z] |
         [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] |
         [#x370-#x37D] | [#x37F-#x1FFF] |
         [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
         [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
         [#x10000-#xEFFFF]
   *)
  let is_name_start_char c = ( (c==59) || (* ; *)
                               (c==95) || (* _ *)
                               ((c>=65) && (c<=90)) || (* A to Z *)
                               ((c>=97) && (c<=122)) || (* a to z *)
                               ((c>=0xc0) && (c<=0xd6)) ||
                               ((c>=0xd8) && (c<=0xf6)) ||
                               ((c>=0xf8) && (c<=0x2ff)) ||
                               ((c>=0x370) && (c<=0x37d)) ||
                               ((c>=0x37f) && (c<=0x1fff)) ||
                               ((c>=0x200c) && (c<=0x200d)) ||
                               ((c>=0x2070) && (c<=0x218f)) ||
                               ((c>=0x2c00) && (c<=0x2fef)) ||
                               ((c>=0x3001) && (c<=0xd7ff)) ||
                               ((c>=0xf900) && (c<=0xfdcf)) ||
                               ((c>=0xfdf0) && (c<=0xfffd)) ||
                               ((c>=0x10000) && (c<=0xeffff)) ||
                               false )

  (*f is_name_char c - definition of Name, [4a] Sect 2.3
  [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
   *)
  let is_name_char c = ( (is_name_start_char c) ||
                             (c==124) || (* | *)
                             (c==44)  || (* , *)
                             ((c>=48) && (c<=57)) || (* 0 to 9 *)
                             (c==0xb7) ||
                             ((c>=0x300) && (c<=0x36f)) ||
                             ((c>=0x203f) && (c<=0x2040)) ||
    
    false )

  (*f is_char_data c - definition of character data (the non-XML of a document), [14] Sect 2.4
   [14]    CharData    ::=    [^<&]* - ([^<&]* ']]>' [^<&]* )
    This means any string of characters not including < or &, and also ]]> is banned... but this ignores that
   *)
  let is_char_data c = ( (c!=eof) && (c!=60) && (c!=38) )

  (*f bad_unicode - mark as a bad unicode stream *)
  let bad_unicode i = 
    i.bad_unicode <- true

  (*f unget_unicode - unget unicode character from stream *)
  let unget_unicode i uc =
    i.unget_unicode <- Some uc

  (*f get_unicode - get unicode character from stream or unget *)
  let get_unicode i =
    match i.unget_unicode with
    Some uc -> (i.unget_unicode <- None; uc)
  | None    -> Utf8.getc i.f

  (*f get_unbuf - get character accounting for newlines Sect 2.11 *)
  let get_unbuf i =
    let uc = get_unicode i in
    let (s,c) = Utf8.sc uc in
    let newline _ = (i.column <- 0; i.line <- (i.line + 1); Utf8.newline) in
    if (c=0x0a) then newline ()
    else if (c==0x85) then newline ()
    else if (c==0x2028) then newline ()
    else if (c==0xd) then ( (* 0xd.0xa 0xd.0x85 0xd.* *)
      let nuc = get_unicode i in
      let (ns, nc) = Utf8.sc nuc in
      if ((nc<>0xa) && (nc<>0x85)) then unget_unicode i nuc;
      newline ()
    )
    else ( i.column <- i.column+1; uc)

  (*f unget - unget a unicode character *)
  let unget i uc =
    i.unget_buffer <- uc :: i.unget_buffer

  (*f get - get the next unicode character accounting for newlines according to the spec *)
  let get i = 
    match i.unget_buffer with
      [] -> (get_unbuf i)
    | uc::rest -> (i.unget_buffer <- rest; uc)

  (*f build_string - accumulate a string while the unicode characters match a filter function *)
  let rec build_string i acc filter =
    let uc = get i in
    let (s,c) = Utf8.sc uc in
    if (not (filter c)) then
      (unget i uc ;
       acc)
    else
      let nacc = acc ^ s in
      (build_string i nacc filter)

  (*f skip_while - skip while the unicode characters match a filter function *)
  let rec skip_while i acc filter =
    let uc = get i in
    let (_,c) = Utf8.sc uc in
    if (not (filter c)) then
      (unget i uc ;
       acc)
    else
      (skip_while i true filter)

  (*f skip_whitespace - return true if anything consumed, false otherwise *)
  let skip_whitespace i = skip_while i false is_whitespace

  (*f debug_string - attempt to read characters up to the string length, return true if match, otherwise unget and return false *)
  let debug_string i len = 
    let rec add_element acc =
      let nuc = get i in
      let (ns,nc) = Utf8.sc nuc in
      let nacc = acc ^ ns in
      let l = String.length nacc in
      if (nc==eof) then nacc
      else if (l>=len) then (unget i nuc;nacc)
      else (
        let r = add_element nacc in
        unget i nuc;
        r)
    in
    add_element ""

  (*f match_string - attempt to read characters up to the string length, return true if match, otherwise unget and return false *)
  let match_string i st = 
    let sl = String.length st in
    let rec add_element_and_check acc =
      let nuc = get i in
      let (ns,nc) = Utf8.sc nuc in
      let nacc = acc ^ ns in
      let l = String.length nacc in
      if (nc==eof) then false
      else if ((l==sl) && (nacc=st)) then true
      else if (l>=sl) then (unget i nuc;false)
      else if (add_element_and_check nacc) then true
      else (unget i nuc;false)
    in
    add_element_and_check ""

  (*f read_quoted_string - read a single or double quote, then characters up to a repeat of that quote; return Some string, or None
    do not skip whitespace to start with
   *)
  let read_quoted_string i =
    let opt_s = if (match_string i "\'") then
      Some (build_string i "" (fun c->(c<>39))) (* single quote *)
    else if (match_string i "\"") then
      Some (build_string i "" (fun c->(c<>34))) (* double quotes *)
    else None
    in
    if (option_is_some opt_s) then ignore (get i);
    opt_s

  (*f read_whitespace - read whitespace, keeping it in a string, stopping at non-whitespace [3] Sect 2.3 *)
  let read_whitespace i =  (build_string i "" is_whitespace)

  (*f read_name - read a name, keeping it in a string, stopping if not a name_start_char or not a name_char [5] Sect 2.3
    [5] Name ::= NameStartChar (NameChar)*
   *)
  let read_name i =
    let uc = get i in
    let (s,c) = Utf8.sc uc in
    if (not (is_name_start_char c)) then
      (unget i uc ; "")
    else
      (build_string i s is_name_char)

  (*f parse_comment [15] - return Error, or Ok false (if nothing skipped) or Ok true (if something skipped)
    [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    <!-- stuff -->  (with no more than 1 - in a row inside the comment)
  *)
  let parse_comment i =
    if (not (match_string i "<!--")) then (Ok false)
    else
      let last_was_dash = ref false in
      let is_not_second_dash c =
        if (c==eof) then false
        else if (c<>45 (*'-'*) ) then (last_was_dash := false; true)
        else if (!last_was_dash) then false
        else (last_was_dash := true; true)
      in
      let s = (build_string i "" is_not_second_dash) in
      (ignore s);
      if (not (match_string i "->")) then (malformed_xml_error i "Badly terminated comment")
      else (* String.sub s 0 ((String.length s)-1) *)
        (Ok true)

  (*f parse_processing_instruction [16-17] - invoke handler; return Error, or Ok false (if nothing skipped) or Ok true (if something skipped)
    [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char* )))? '?>'
    [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    <?<name that is not xml> <stuff that is not '?>'> ?>
  *)
  let parse_processing_instruction i =
    if (not (match_string i "<?")) then (Ok false)
    else
      let target = read_name i in
      if (target="") then  (malformed_xml_error i "Processing instruction must be <?Name")
      else if (((String.length target)>=3) &&
                 ((target.[0]=='X')||(target.[0]=='x')) &&
                   ((target.[1]=='M')||(target.[1]=='m')) &&
                     ((target.[2]=='L')||(target.[2]=='l'))) then (malformed_xml_error i "Processing instruction must not start with XML")
      else 
        let last_was_query = ref false in
        let complete       = ref false in
        let is_not_closed c =
          if (c==eof) then false
          else if (!complete) then false
          else if (!last_was_query && (c==62 (*'>'*) )) then (complete := true; true)
          else if (c=63 (*'?'*) ) then (last_was_query := true; true)
          else (last_was_query := false; true)
        in
        let pi_plus_two = (build_string i "" is_not_closed) in
        let pi_len_plus_two = String.length pi_plus_two in
        if (pi_len_plus_two<2) then (malformed_xml_error i "Bad processing instruction")
        else
          let pi = String.sub pi_plus_two 0 (pi_len_plus_two-2) in
          ch_invoke i i.content_handler.Content_handler.pi_callback (fun f x -> f x target pi);
          (Ok true)

  (*f parse_cdata [18-21] - invoke handler; return Error, or Ok false (if nothing skipped) or Ok true (if something skipped)
    [18]    CDSect    ::=    CDStart CData CDEnd
    [19]    CDStart    ::=    '<![CDATA['
    [20]    CData    ::=    (Char* - (Char* ']]>' Char* ))
    [21]    CDEnd    ::=    ']]>'
  *)
  let parse_cdata i =
    if (not (match_string i "<![CDATA[")) then (Ok false)
    else
      let stage_of_end = ref 0 in
        let complete       = ref false in
      let is_not_closing c =
        if (c==eof) then false
          else if (!complete) then false
        else if ( (c=62 (*'>'*)) && (!stage_of_end=2) ) then (complete := true;true)
        else if (c<>93 (* ']' *) ) then (stage_of_end := 0; true)
        else if (!stage_of_end=2) then true
        else (stage_of_end := (!stage_of_end)+1; true)
      in
      let cdata_plus_three = (build_string i "" is_not_closing) in
        let cdata_len_plus_three = String.length cdata_plus_three in
        if (cdata_len_plus_three<3) then (malformed_xml_error i "Bad cdata")
        else (
          (ch_invoke i i.content_handler.Content_handler.characters_callback (fun f x -> f x cdata_plus_three 0 (cdata_len_plus_three-3)));
          (Ok true)
        )

  (*f parse_attribute - parse an attribute=value, returning Some (attribute,value); returning None if not name; or raising exception for bad name=value
  [41]   Attribute    ::=    Name Eq AttValue
   *)
  let validate_attr_value s = true
  let parse_attribute i =
    let name = read_name i in
    if (name="") then None
    else (
       if (not (match_string i "=")) then (malformed_xml i "Expected = in attribute assignment")
       else
         let opt_value = read_quoted_string i in
         if (option_is_none opt_value) then (malformed_xml i "Expected quoted value in attribute assignment")
         else
           let value = option_get opt_value in
           if (not (validate_attr_value value)) then (malformed_xml i (sfmt "Invalid attribute value '%s'" value))
           else
             Some (Attributes.create ~qName:name ~value:value ~uri:"" ~localName:"" ())
    )

  (*f parse_attributes - skipping whitespace, reading all attributes
    (S Attribute)* S?
   *)
  let rec parse_attributes i acc =
    ignore (skip_whitespace i);
    let opt_nv = (parse_attribute i)
    in
    if (option_is_none opt_nv) then acc
    else
      let nacc = acc@[option_get opt_nv] in
      parse_attributes i nacc

  (*f parse_xml_decl - [23]
   [23]   	XMLDecl	   ::=   	'<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    <?xml [version='1.1'|version="1.1"] [encoding=["<EncName>"|'<EncName>']] [standalone=['yes'|"yes"|'no'|"no"]]?>
   *)
  let parse_xml_decl i =
    if (not (match_string i "<?xml")) then
      false
    else
      (
        (*let nvs = parse_attributes i [] in*)
        if (not (match_string i "?>")) then
          (malformed_xml i "Badly terminated <?xml declaration - expected ?>")
        else 
        (*List.iter nvs check_for_version_encoding_standalone; *)
        true
      )

  (*f parse_misc - [27] Misc ::= Comment | PI | S 
    Return false if nothing parsed, handle callbacks if required
  *)
  let parse_misc i =
    (Ok (skip_whitespace i)) >>= fun ws_done ->
    parse_processing_instruction i >>= fun pi_done ->
    parse_comment                i >>= fun cm_done ->
    Ok (ws_done || pi_done || cm_done)

  (*f parse_all_misc - Parse Misc*, with result
   *)
  let rec parse_all_misc i =
    match (parse_misc i) with
      Error _ as e -> e
    | Ok more -> if (not more) then
                   (Ok ())
                 else
                   parse_all_misc i

  (*f parse_dtd - really complex, punting for now
[28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
   *)
  let parse_dtd i =
    if (not (match_string i "<!DOCTYPE")) then Ok ()
    else (
    ignore (skip_whitespace i);
    let root_name = read_name i in (* Required, should be the root element name *)
    ignore root_name;
    ignore (skip_whitespace i);
     Ok ()
 (* ExternalID is possible; after that (whitespace) [ intSubset]; after that >
[28b] 	intSubset	   ::=   	(markupdecl | DeclSep)*
[28a]  	DeclSep	   ::=   	PEReference (%Name;) | S - PEReference should be expanded
[29]   	markupdecl	   ::=   	elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment	[VC: Proper Declaration/PE Nesting]
[30]   	extSubset	   ::=   	TextDecl? extSubsetDecl
[31]   	extSubsetDecl	   ::=   	( markupdecl | conditionalSect | DeclSep)*
[32]   	SDDecl	   ::=   	S standalone = "yes"|"no"|'yes'|'no'
 *)
    )

  (*f parse_prolog - parse the prolog (xml declaration, comments, processing instructions, dtd) [22]
    [22] prolog ::= XMLDecl Misc* (doctypedecl Misc* )?
    Any number of misc; only one dtd
   *)
  let parse_prolog i =
    let has_xml_decl = parse_xml_decl i in
    ignore has_xml_decl;
    parse_all_misc i >>= fun _ ->
    parse_dtd i      >>= fun _ ->
    parse_all_misc i

  (*f rec parse_content - parse one item of content and invoke appropriate callback
    This is between tags, so whitespace is ignoreable
[43]   	content	   ::=   	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
 element starts with <; Reference with &; CDSect is <!CDATA; PI is <?; comment is <!--
  hence CharData? is a sequence of is_char_data
   *)
  let rec parse_content i =
    let cdata = (build_string i "" is_char_data) in
    if (cdata<>"") then (*invoke i i.cdata_callback;*) (Ok true)
    else
      let nuc = get i in
      let (ns,nc) = Utf8.sc nuc in
      unget i nuc ;
      if (nc=60) then
        (
          parse_processing_instruction i >>= fun pi_done ->
          parse_comment                i >>= fun cm_done ->
          parse_cdata                  i >>= fun cd_done ->
          parse_element          i false >>= fun el_done ->
          (Ok (pi_done || cm_done || cd_done || el_done) )
        )
      else (malformed_xml_error i "Expected cdata, < or &")

  (*f and rec parse_all_content
 [43]   	content	   ::=   	CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
  *)
  and parse_all_content i =
    match (parse_content i) with
      Error _ as e -> e
    | Ok more -> if (not more) then
                   (Ok ())
                 else
                   parse_all_content i

  (*f and rec parse_element - parse an element [39]
[39]   	element	   ::=   	EmptyElemTag | STag content ETag

[40]   	STag	   ::=   	'<' Name (S Attribute)* S? '>'
[42]   	ETag	   ::=   	'</' Name S? '>'
[44]   	EmptyElemTag	   ::=   	'<' Name (S Attribute)* S? '/>'	[WFC: Unique Att Spec]
   *)
  and parse_element i root =
    let nuc1  = get i in
    let nuc2 = get i in
    let (_,nc1) = Utf8.sc nuc1 in
    let (_,nc2) = Utf8.sc nuc2 in
    unget i nuc2 ;
    unget i nuc1 ;
    if ((nc1<>60) || (not (is_name_start_char nc2))) then
      (Ok false)
    else
      let open_tag _ =
        if (not (match_string i "<")) then (malformed_xml_error i "Expected < to start tag")
        else
          let name = read_name i in
          if (name = "") then (malformed_xml_error i "Expected <name at start of tag")
          else (Ok name)
      in
      let attributes _ =
        Ok (parse_attributes i [])
      in
      let close_tag name =
        if (not (match_string i "</")) then (malformed_xml_error i (sfmt "Expected end tag </%s>" name))
        else if (not (match_string i name)) then (malformed_xml_error i (sfmt "Expected end tag </%s>" name))
        else
          (
            ignore (skip_whitespace i);
            if (not (match_string i ">")) then (malformed_xml_error i (sfmt "Expected end tag </%s>" name))
            else (Ok ())
          )
      in
      let element name =
        if (match_string i "/>") then (Ok ())
        else if (not (match_string i ">")) then (malformed_xml_error i "Expected closing > at end of tag")
        else
          match (parse_all_content i) with
            Error _ as e -> e
          | Ok _ -> (close_tag name)
      in
      open_tag () >>= fun name ->
      attributes () >>= fun atts ->
      if (root) then (ch_invoke i i.content_handler.Content_handler.start_document_callback (fun f x -> f x ~rootName:name));
      ch_invoke i i.content_handler.Content_handler.start_element_callback (fun f x -> f x ~uri:"" ~localName:name ~qName:"" ~atts:atts);
      element name >>= fun _ ->
      ch_invoke i i.content_handler.Content_handler.end_element_callback (fun f x -> f x ~uri:"" ~localName:name  ~qName:"");
      if (root) then (ch_invoke i i.content_handler.Content_handler.end_document_callback (fun f x -> f x));
      Ok true

  (*f let parse_root_element - parse an element as root - it MUST be there...
   *)
  let parse_root_element i =
    parse_element i true >>= fun root_found ->
    if (root_found) then
     (Ok true)
    else
     (malformed_xml_error i "Root element not found")

  (*f parse_stream [1]
[1]    document    ::=    ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
   *)
  let parse_stream t s =
    set_stream t s ;
    parse_prolog t  >>= fun _ ->
    parse_root_element t >>= fun _ ->
    Ok ()

  (*f parse_string - parse a stream consisting of the characters of the stream
   *)
  let parse_string t s = 
    parse_stream t (Stream.of_string s)

  (*f All done *)
end

(*f Unsorted *)

(*a Notes

[6]    Names    ::=    Name (#x20 Name)*
[7]    Nmtoken    ::=    (NameChar)+
[8]    Nmtokens    ::=    Nmtoken (#x20 Nmtoken)*
[9]    EntityValue    ::=    '"' ([^%&"] | PEReference | Reference)* '"'
|  "'" ([^%&'] | PEReference | Reference)* "'"
[10]    AttValue    ::=    '"' ([^<&"] | Reference)* '"'
|  "'" ([^<&'] | Reference)* "'"
[11]    SystemLiteral    ::=    ('"' [^"]* '"') | ("'" [^']* "'")
[12]    PubidLiteral    ::=    '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
[13]    PubidChar    ::=    #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
[24]    VersionInfo    ::=    S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
[25]    Eq    ::=    S? '=' S?
[26]    VersionNum    ::=    '1.1'

[28]    doctypedecl    ::=    '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>' [VC: Root Element Type]
[28a]    DeclSep    ::=    PEReference | S [WFC: PE Between Declarations]
[28b]    intSubset    ::=    (markupdecl | DeclSep)*
[29]    markupdecl    ::=    elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment [VC: Proper Declaration/PE Nesting]
[30]    extSubset    ::=    TextDecl? extSubsetDecl
[31]    extSubsetDecl    ::=    ( markupdecl | conditionalSect | DeclSep)*
[32]    SDDecl    ::=    S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"')) [VC: Standalone Document Declaration]
 EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')*



[45]    elementdecl    ::=    '<!ELEMENT' S Name S contentspec S? '>' [VC: Unique Element Type Declaration]
[46]    contentspec    ::=    'EMPTY' | 'ANY' | Mixed | children
[47]    children    ::=    (choice | seq) ('?' | '*' | '+')?
[48]    cp    ::=    (Name | choice | seq) ('?' | '*' | '+')?
[49]    choice    ::=    '(' S? cp ( S? '|' S? cp )+ S? ')' [VC: Proper Group/PE Nesting]
[50]    seq    ::=    '(' S? cp ( S? ',' S? cp )* S? ')' [VC: Proper Group/PE Nesting]

[51]    Mixed    ::=    '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
| '(' S? '#PCDATA' S? ')' [VC: Proper Group/PE Nesting]
[VC: No Duplicate Types]

[52]    AttlistDecl    ::=    '<!ATTLIST' S Name AttDef* S? '>'
[53]    AttDef    ::=    S Name S AttType S DefaultDecl

[54]    AttType    ::=    StringType | TokenizedType | EnumeratedType
[55]    StringType    ::=    'CDATA'
[56]    TokenizedType    ::=    'ID'
| 'IDREF' 
| 'IDREFS' 
| 'ENTITY' 
| 'ENTITIES'
| 'NMTOKEN' 
| 'NMTOKENS'

[57]    EnumeratedType    ::=    NotationType | Enumeration
[58]    NotationType    ::=    'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')' [VC: Notation Attributes]
[59]    Enumeration    ::=    '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')' [VC: Enumeration]

[60]    DefaultDecl    ::=    '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue) 

[61]    conditionalSect    ::=    includeSect | ignoreSect
[62]    includeSect    ::=    '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>' [VC: Proper Conditional Section/PE Nesting]
[63]    ignoreSect    ::=    '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>' [VC: Proper Conditional Section/PE Nesting]
[64]    ignoreSectContents    ::=    Ignore ('<![' ignoreSectContents ']]>' Ignore)*
[65]    Ignore    ::=    Char* - (Char* ('<![' | ']]>') Char* )

[66]    CharRef    ::=    '&#' [0-9]+ ';'
| '&#x' [0-9a-fA-F]+ ';' [WFC: Legal Character]

[67] Reference    ::=    EntityRef | CharRef
[68]    EntityRef    ::=    '&' Name ';'
[69]    PEReference    ::=    '%' Name ';'

[70]    EntityDecl    ::=    GEDecl | PEDecl
[71]    GEDecl    ::=    '<!ENTITY' S Name S EntityDef S? '>'
[72]    PEDecl    ::=    '<!ENTITY' S '%' S Name S PEDef S? '>'
[73]    EntityDef    ::=    EntityValue | (ExternalID NDataDecl?)
[74]    PEDef    ::=    EntityValue | ExternalID

[75]    ExternalID    ::=    'SYSTEM' S SystemLiteral
| 'PUBLIC' S PubidLiteral S SystemLiteral
[76]    NDataDecl    ::=    S 'NDATA' S Name [VC: Notation Declared]

[77]    TextDecl    ::=    '<?xml' VersionInfo? EncodingDecl S? '?>'

[78]    extParsedEnt    ::=    ( TextDecl? content ) - ( Char* RestrictedChar Char* )

[80]    EncodingDecl    ::=    S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
[81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')*

[82]    NotationDecl    ::=    '<!NOTATION' S Name S (ExternalID | PublicID) S? '>' [VC: Unique Notation Name]
[83]    PublicID    ::= 'PUBLIC' S PubidLiteral
Sect 2.3: A Name is a token beginning with a letter or one of a few punctuation characters, and continuing with letters, digits, hyphens, underscores, colons, or full stops, together known as name characters.]
Any Name starting with XML (with letters of any case) is reserved.



<?xml ?>
<app>
<window>
<
</window class="">
</app>
 *)
