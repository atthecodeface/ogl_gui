(** Copyright (C) 2017-2018,  Gavin J Stark.  All rights reserved.
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
 * @file    stylesheet.ml
 * @brief   A library for having cascading stylesheets
 *
 *)

(*a Notes
CSS notes

font
-family - comma separated list of font names
-size - size or percentage of inheritance
-weight
-stretch
[-thickness?]
line-height

vertical-align

max-width

margin 0 auto !important

padding (top, bottom, left, right)

border (top, bottom, left, right)
border-color
border-style
border-width

box-sizing

display


background

color


*)

(*a Helper functions and modules *)
(*f sfmt *)
let sfmt = Printf.sprintf

(*f read_floats *)
let string_as_float_rex =
  let point = Re.char '.' in
  let minus = Re.char '-' in
  let digit = Re.set "0123456789" in
  let float = Re.group (Re.seq [ Re.opt minus; Re.rep1 digit ; Re.opt point ; Re.rep digit] ) in
  let rest  = Re.group (Re.rep Re.any) in
  let opt_whitespace = Re.rep Re.space in
  Re.compile (Re.seq [opt_whitespace ; float ; rest])

(*f string_as_int_rex *)
let string_as_int_rex =
  let prefix = Re.seq [ Re.char '0' ; Re.set "xX" ] in
  let number = Re.group (Re.seq [ Re.opt prefix; Re.rep1 (Re.set "0123456789abcdefABCDEF")] ) in
  let rest  = Re.group (Re.rep Re.any) in
  let opt_whitespace = Re.rep Re.space in
  Re.compile (Re.seq [opt_whitespace ; number ; rest])

(*f extract_first_and_rest *)
let extract_first_and_rest rex s =
  match (Re.exec_opt rex s) with
    None -> None
  | Some g -> Some ((Re.Group.get g 1),(Re.Group.get g 2))
    
(*f fill_array *)
let rec fill_array a num n i j =
  if (i=num) then
    a
  else
    (
      a.(i) <- a.(j);
      let next_j = if (j+1=n) then 0 else (j+1) in
      fill_array a num n (i+1) next_j
    )

(*f read_floats *)
let read_floats string num = 
  let floats = Array.make num 0. in
  let rec read_floats_from_n n string =
    if (n=num) then
      n
    else
      (
        match extract_first_and_rest string_as_float_rex string with
          None -> n
        | Some s12 -> 
           (
             let (s1,s2) = s12 in
             floats.(n) <- float_of_string s1;
             read_floats_from_n (n+1) s2
           )
      )
  in
  let n = read_floats_from_n 0 string in
  fill_array floats num (max n 1) n 0

(*f read_ints *)
let read_ints string num = 
  let ints = Array.make num 0 in
  let rec read_ints_from_n n string =
    if (n=num) then
      n
    else
      (
        match extract_first_and_rest string_as_int_rex string with
          None -> n
        | Some s12 -> 
           (
             let (s1,s2) = s12 in
             ints.(n) <- int_of_string s1;
             read_ints_from_n (n+1) s2
           )
      )
  in
  let n = read_ints_from_n 0 string in
  fill_array ints num (max n 1) n 0

(*a Styleable_value module *)
(*m Styleable_value *)
module Styleable_value =
  struct
    (*t t_styleable_name *)
    type t_styleable_name = string

    (*t t_styleable_value *)
    type t_styleable_value =
      Sv_float_6 of float array
    | Sv_float_3 of float array
    | Sv_float of float
    | Sv_int  of int
    | Sv_int_3 of int array
    | Sv_int_6 of int array
    | Sv_rgb of float array

    (*t t_styleable_type *)
    type t_styleable_type = St_float_6 | St_float_3 | St_float | St_int | St_int_3 | St_int_6 | St_rgb

    (*v sv_zero, sv_none - for use as defaults *)
    let sv_zero = Sv_int 0
    let sv_none = Sv_int 0

    (*v one_i_0, one_f_0 - useful float arrays as defaults *)
    let one_i_0 = [|0;|]
    let one_f_0 = [|0.;|]

    (*f stype_of_svalue - find type of an svalue *)
    let stype_of_svalue v = 
      match v with
        Sv_float _ -> St_float
      | Sv_float_3 _ -> St_float_3
      | Sv_float_6 _ -> St_float_6
      | Sv_int _ -> St_int
      | Sv_int_6 _ -> St_int_6
      | Sv_int_3 _ -> St_int_3
      | Sv_rgb _ -> St_rgb

    (*f str_of_svalue - generate string of an svalue *)
    let str_of_svalue v =
      match v with
        Sv_float f   -> sfmt "flt:%f" f
      | Sv_float_3 f -> sfmt "f3:%f %f %f" f.(0) f.(1) f.(2)
      | Sv_float_6 f -> sfmt "f6:%f %f %f %f %f %f" f.(0) f.(1) f.(2) f.(3) f.(4) f.(5)
      | Sv_int i     -> sfmt "int:%d" i
      | Sv_int_3 i   -> sfmt "i3:%d %d %d"          i.(0) i.(1) i.(2)
      | Sv_int_6 i   -> sfmt "i6:%d %d %d %d %d %d" i.(0) i.(1) i.(2) i.(3) i.(4) i.(5)
      | Sv_rgb f     -> sfmt "rgb:%f %f %f" f.(0) f.(1) f.(2)

    (*f svalue_from_string *)
    let svalue_from_string stype value =
      match stype with
        St_float   -> ( let floats = read_floats value 1 in Sv_float floats.(0) )
      | St_float_3 -> ( let floats = read_floats value 3 in Sv_float_3 floats )
      | St_rgb     -> ( let floats = read_floats value 3 in Sv_rgb floats )
      | St_float_6 -> ( let floats = read_floats value 6 in Sv_float_6 floats )
      | St_int     -> ( let ints = read_ints  value 1 in Sv_int ints.(0) )
      | St_int_3   -> ( let ints = read_ints  value 3 in Sv_int_3 ints )
      | St_int_6   -> ( let ints = read_ints  value 6 in Sv_int_6 ints )

    (*f svalue_as_floats - get a float array of an svalue *)
    let svalue_as_floats svalue =
      match svalue with
        Sv_float_3 f -> f
      | Sv_rgb     f -> f
      | Sv_float_6 f -> f
      | Sv_float   f -> one_f_0
      | Sv_int     i -> one_f_0
      | Sv_int_3   i -> one_f_0
      | Sv_int_6   i -> one_f_0

    (*f svalue_as_float - get a float for an svalue *)
    let svalue_as_float svalue =
      match svalue with
        Sv_float_3 f -> f.(0)
      | Sv_rgb     f -> f.(0)
      | Sv_float_6 f -> f.(0)
      | Sv_float   f -> f
      | Sv_int     i -> float i
      | Sv_int_3   i -> float i.(0)
      | Sv_int_6   i -> float i.(0)

    (*f svalue_as_ints - get an int array for an svalue *)
    let svalue_as_ints svalue =
      match svalue with
        Sv_float_3 f -> one_i_0
      | Sv_rgb     f -> one_i_0
      | Sv_float_6 f -> one_i_0
      | Sv_float   f -> one_i_0
      | Sv_int     i -> one_i_0
      | Sv_int_3   i -> i
      | Sv_int_6   i -> i

    (*m Styleable_value_ref *)
    module Styleable_value_ref = struct 
      type t_styleable_value_ref = 
        Ref of t_styleable_value
      | Default
      | Inherit

      (*t t - structure for a reference to a t_styleable_value *)
      type t = { 
          mutable longest_rule   : int;
          mutable next_value_ref : t_styleable_value_ref;
          mutable value          : t_styleable_value;
          mutable default_value  : t_styleable_value;
          mutable default_inherit : bool;
        }

      (*f create - create a null value reference *)
      let create _ = {
          longest_rule = 0;
          next_value_ref = Default;
          value=sv_zero;
          default_value=sv_none;
          default_inherit = false;
        }

      (*f get_value t - get the value *)
      let get_value t = t.value

      (*f next_value_ref *)
      let next_value_ref t = 
        t.next_value_ref 

      (*f get_value_as_floats t - get the value as an array of floats *)
      let get_value_as_floats t =  svalue_as_floats t.value

      (*f get_value_as_float t - get the value as a float *)
      let get_value_as_float t  =  svalue_as_float  t.value

      (*f get_value_as_ints t - get the value as an array of ints *)
      let get_value_as_ints t   =  svalue_as_ints   t.value

      (*f set_default_inherit - set the default inheritance *)
      let set_default_inherit t di = t.default_inherit <- di

      (*f set_value *)
      let set_value t v = t.value <- v

      (*f set_default_from_string *)
      let set_default_from_string t stype value =
        t.default_value <- svalue_from_string stype value

      (*f reset *)
      let reset t =     
        t.longest_rule <- 0;
        if (t.default_value == sv_none) then
          (
            if (t.default_inherit) then 
              t.next_value_ref <- Inherit
            else
              t.next_value_ref <- Default
          )
        else
          (
            t.longest_rule <- 100;
            t.next_value_ref <- Ref t.default_value
          )

      (*f apply - apply a rule *)
      let apply l sv t  =
        if (t.longest_rule < l) then (
          t.next_value_ref <- Ref sv;
          t.longest_rule <- l
        )

      (*f All done *)
    end

    (*v svr_zero - default null value reference *)
    let svr_zero = Styleable_value_ref.create ()
                                             
    let ref_value            = Styleable_value_ref.get_value
    let ref_value_as_floats  = Styleable_value_ref.get_value_as_floats
    let ref_value_as_float   = Styleable_value_ref.get_value_as_float
    let ref_value_as_ints    = Styleable_value_ref.get_value_as_ints
end
    
(*a Style_id_hash, Style_id, Style_ids, Style, Styleable_desc - immutable after construction, but could be fully immutable *)
(*m Style_id_hash - immutable *)
module Style_id_hash = struct
  type t = string
  let of_string s = s
end

(*m Style_id - immutable *)
module Style_id = struct
  type t = {
      hash : Style_id_hash.t;
      name : string;
      stype : Styleable_value.t_styleable_type;
    }
  let dummy = {name=""; hash=(Style_id_hash.of_string ""); stype=St_int; }
  let create name stype = { name; hash=(Style_id_hash.of_string name); stype; }
  let get_type t = t.stype
  let str t =
    sfmt "sid %s" t.name
end

(*m Style_ids - immutable, but contains a hash table of Style_id_hash.t -> Style_id.t *)
module Style_ids = struct
  exception Unknown_id of string
  exception Duplicate_id
  type t = {
      set : ( Style_id_hash.t , Style_id.t) Hashtbl.t;
    }
  let create _ =
    {
      set = Hashtbl.create 1024;
    }
  let find_opt_id hash t =
    if (Hashtbl.mem t.set hash) then
      Some (Hashtbl.find t.set hash)
    else
      None
  let find_id_exn hash t =
    match find_opt_id hash t with
      None -> raise (Unknown_id "hashed thing")
    | Some sid -> sid
  let add_id hash sid t =
    if (Hashtbl.mem t.set hash) then
      raise Duplicate_id
    else
      Hashtbl.replace t.set hash sid
  let build_id_value_list nvs t = 
    let rec add_id_value acc n_x = 
      let (name,x)=n_x in
      let hash = Style_id_hash.of_string name in
      let opt_sid = find_opt_id hash t in
      match opt_sid with
        None -> raise (Unknown_id name)
      | Some sid -> (sid,x)::acc
    in
    List.fold_left add_id_value [] nvs
end

(*m Style - mutable during construction of default - should probably be immutable *)
module Style = struct
  type t = {
      mutable styles : (Style_id.t * (Styleable_value.t_styleable_value * bool)) list
    }
  let create styles = { styles; }
  let add_styling sid value opt t =
    t.styles <- (sid,(value,opt))::t.styles
  let str t =
    let str_svo acc svo =
      let (sid,(svalue,opt)) = svo in
      sfmt "%s%s:%s:%b\n" acc (Style_id.str sid) (Styleable_value.str_of_svalue svalue) opt
    in
    List.fold_left str_svo "style:\n" t.styles
  let get_value sid t =
    fst (List.assoc sid t.styles)
  let get_opt sid t =
    snd (List.assoc sid t.styles)
end

(*a Stylesheet module *)
module Stylesheet =
  struct
    (*t t_styleable_name *)
    type t_styleable_name = Styleable_value.t_styleable_name

    (*t t_styleable_value *)
    type t_styleable_value = Styleable_value.t_styleable_value

    (*t t_styleable_type *)
    type t_styleable_type = Styleable_value.t_styleable_type

    (*m Styleable_desc - immutable *)
    module Styleable_desc = struct
      (*t Structure *)
      type t= {
          state_descriptor : ( string * ((string * int) list)) list; (* each entry is a state_class -> (list of state_name of state_class->int) mappings *)
          styles : (string * t_styleable_type) list; (* List of all stylenames the styleable cares about; usually a static list *)
        }

      (*f create *)
      let create state_descriptor styles =
        {
          state_descriptor;
          styles;
        }

      (*f All done *)
    end

    (*m Styleable_desc_built -  immutable *)
    module Styleable_desc_built = struct
      (*t Structure *)
      type t= {
          desc : Styleable_desc.t;
          sids : Style_id.t array; (* Array of all style_ids the styleable cares about; created after the stylesheet is set up (effectively binds the desc to the stylesheet) *)
        }

      (*f get_nth_sid *)
      let get_nth_sid n t = t.sids.(n)

      (*f create *)
      exception Style_id_not_found_in_binding of string
      exception Style_type_mismatch of string
      let create desc ids =
        let fn acc n_t =
          let (name,stype) = n_t in
          let opt_id = (Style_ids.find_opt_id (Style_id_hash.of_string name) ids) in
          match opt_id with
            None -> raise (Style_id_not_found_in_binding name)
          | Some sid -> 
             if (sid.Style_id.stype != stype) then raise (Style_type_mismatch name)
             else
               sid :: acc
        in
        let t = {
            desc;
            sids=Array.of_list (List.fold_left fn [] desc.Styleable_desc.styles);
          }
        in
        Printf.printf "Bind to ids\n";
        Array.iter (fun sid -> Printf.printf "%s\n" (Style_id.str sid)) t.sids;
        t

      (*f find_sid_index  *)
      let find_sid_index sid t =
        let find_sid acc i =
          let (n, opt_res) = acc in
          if (i=sid) then (n+1, Some n)
          else (n+1, opt_res)
        in
        let (_,opt_index) = Array.fold_left find_sid (0,None) t.sids in
        opt_index

      (*f find_sid_index_exn  *)
      let find_sid_index_exn sid t =
        match find_sid_index sid t with
          None -> raise (Style_id_not_found_in_binding (Style_id.str sid))
        | Some index -> index

      (*f All done *)
    end

    (*a Styleable and Stylesheet - immutable post-construction *)
    (*m Styleable - mutable parent, style_change_callback; else immutable *)
    module rec Styleable : sig
             (*m Signature *)
             (*t structure and t_style_change_callback *)
             type t_style_selector = (t -> bool)
             and t_style_change_callback = (Style_id.t * t_styleable_value) list -> unit
             and t_style_change_fn = t -> unit (* internal apply style rule *)
             and t= {
                 sheet      : Stylesheet.t;
                 desc_built : Styleable_desc_built.t;
                 num_base_styles : int;
                 num_styles : int;
                 extra_sids   : Style_id.t array;
                 children : t list;
                 mutable parent : t option;
                 state : int array;
                 style_change_callback : t_style_change_callback;
                 id_name : string;
                 type_name : string;
                 classes : string list;
                 values    : Styleable_value.Styleable_value_ref.t array; (* next_values.(i) corresponds to desc.sids.(i) *)
               }

             val create : Styleable_desc.t -> Stylesheet.t -> string -> (string * string) list -> t_style_change_callback -> t list -> t
             val set_parent : t -> t -> unit
             val set_element_state  : int -> int -> t -> unit
             val get_id : t -> string
             val get_type : t -> string
             val is_element_id     : string -> t -> bool
             val is_element_type   : string -> t -> bool
             val has_element_class : string -> t -> bool
             val is_element_state  : int -> int -> t -> bool
             val get_value_ref     : t -> string  -> Styleable_value.Styleable_value_ref.t
             val get_value         : t -> string -> t_styleable_value
             val reset_next_values : t -> unit
             val apply_styles : int -> (Style_id.t * t_styleable_value) list -> t -> unit
             val update_current_values_from_next : t -> unit
             val element_callback_matching_children    : t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit
             val element_callback_matching_subelements : t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit
             val element_callback_matching_tree        : t_style_selector -> t -> Stylesheet.t -> t_style_change_fn -> unit

           end = struct
      (*m Struct *)
      (*t type *)
      type t_style_selector = (t -> bool)
      and t_style_change_callback = (Style_id.t * t_styleable_value) list -> unit
      and t_style_change_fn = t -> unit (* internal apply style rule *)
      and t= {
          sheet : Stylesheet.t;
          desc_built : Styleable_desc_built.t;
          num_base_styles : int;
          num_styles : int;
          extra_sids   : Style_id.t array;
          children : t list;
          mutable parent : t option; (* Cannot make this immutable as we need two-way links for style inheritance *)
          state : int array;
          style_change_callback : t_style_change_callback;
          id_name : string;
          type_name : string;
          classes : string list;
          values    : Styleable_value.Styleable_value_ref.t array; (* next_values.(i) corresponds to desc.sids.(i) *)
        }

      (*f set_parent*)
      let set_parent p t = t.parent <- Some p
                                            
      (*f get_id *)
      let get_id t = t.id_name

      (*f get_type *)
      let get_type t = t.type_name

      (*f get_nth_sid *)
      let get_nth_sid n t = 
        if n<t.num_base_styles then
          Styleable_desc_built.get_nth_sid n t.desc_built
        else
          t.extra_sids.(n-t.num_base_styles)

      (*f find_sid_index *)
      let find_sid_index sid t =
        match (Styleable_desc_built.find_sid_index sid t.desc_built) with
          Some sid -> Some sid
        | None -> (
          let find_sid acc i =
            let (n, opt_res) = acc in
            if (i=sid) then (n+1, Some n)
            else (n+1, opt_res)
          in
          let (_,opt_index) = Array.fold_left find_sid (0,None) t.extra_sids in
          opt_index
        )

      (*f find_sid_index_exn *)
      let find_sid_index_exn sid t =
        match find_sid_index sid t with 
          None -> raise (Styleable_desc_built.Style_id_not_found_in_binding (Style_id.str sid))
        | Some index -> index

      (*f get_value_ref *)
      let get_value_ref t (s:string) =
        let sid = Stylesheet.style_id_of_name_exn s t.sheet in
        let sindex = find_sid_index_exn sid t in
        t.values.(sindex)

      (*f get_value *)
      let get_value t (s:string) =
        Styleable_value.Styleable_value_ref.get_value (get_value_ref t s)

      (*f is_element_id *)
      let is_element_id (s:string) t     =
        (*Printf.printf "Is_Element_Id %s : %s\n" t.id_name s;*)
        (t.id_name = s)

      (*f is_element_type *)
      let is_element_type (s:string) t   =
        (*Printf.printf "Is_Element_Type %s : %s : %s\n" t.id_name s t.type_name;*)
        (t.type_name = s)

      (*f is_element_state *)
      let is_element_state state value t =
        if (state>=(Array.length t.state)) then false
        else (
          (*Printf.printf "Is_Element_State %d %d : %d\n" state value t.state.(state);*)
          t.state.(state) = value
        )

      (*f set_element_state *)
      let set_element_state state value t =
        if (state>=(Array.length t.state)) then ()
        else (t.state.(state) <- value)

      (*f has_element_class *)
      let has_element_class (s:string) t =
        (*Printf.printf "Has_Element_Class %s : %s : %d\n" t.id_name s (List.length t.classes);*)
        (List.mem s t.classes) (* maybe not as this would use ==? *)

      (*f create *)
      let create desc sheet type_name name_values style_change_callback children =
        let desc_built = Stylesheet.build_desc desc sheet in
        let id_name = 
          if (List.mem_assoc "id" name_values) then (List.assoc "id" name_values) else "no_id"
        in
        let classes = 
          let class_str = if (List.mem_assoc "class" name_values) then (List.assoc "class" name_values) else "" in
          let class_list = String.split_on_char ' ' class_str in
          List.filter (fun x->(x<>"")) class_list
        in
        let count_extra_styles acc nv =
          let (name,_) = nv in
          match Stylesheet.style_id_of_name name sheet with
            None -> acc
          | Some sid -> (
            match Styleable_desc_built.find_sid_index sid desc_built with
              None -> (acc+1)
            | Some sid_index -> acc
          )
        in
        let num_extra_styles = List.fold_left count_extra_styles 0 name_values in
        let num_base_styles = (Array.length desc_built.Styleable_desc_built.sids) in
        let num_styles = (num_base_styles+num_extra_styles) in
        let t = {
            sheet;
            desc_built;
            num_base_styles;
            num_styles;
            children;
            style_change_callback;
            id_name;
            parent = None;
            type_name;
            classes;
            extra_sids = Array.make num_extra_styles Style_id.dummy;
            state      = Array.make (List.length desc.Styleable_desc.state_descriptor) 0;
            values     = Array.init num_styles (fun i -> Styleable_value.Styleable_value_ref.create ());
          }
        in
        Stylesheet.add_styleable t sheet;
        let add_extra_style acc nv =
          let (name,_) = nv in
          match Stylesheet.style_id_of_name name t.sheet with
            None -> acc
          | Some sid -> (
            match Styleable_desc_built.find_sid_index sid t.desc_built with
              Some sid_index -> acc
            | None -> (t.extra_sids.(acc) <- sid; acc+1)
          )
        in
        ignore (List.fold_left add_extra_style 0 name_values);
        let set_default_value nv =
          let (name,value) = nv in
          match Stylesheet.style_id_of_name name t.sheet with
            None -> ()
          | Some sid -> (
            match find_sid_index sid t with
              None -> ()
            | Some sid_index -> (
              let stype = Style_id.get_type sid in
              Printf.printf "Set default value of %s.%s.%s to be %s\n" t.id_name t.type_name name value;
              Styleable_value.Styleable_value_ref.set_default_from_string t.values.(sid_index) stype value
            )
          )
        in
        List.iter set_default_value name_values;
        let set_inheritance n vr =
          let sid = get_nth_sid n t in
          let di = (Stylesheet.is_default_inherit sid t.sheet) in
          Styleable_value.Styleable_value_ref.set_default_inherit vr di
        in
        Array.iteri set_inheritance t.values;
        t

      (*f reset_next_values *)
      let rec reset_next_values t =
        Array.iter Styleable_value.Styleable_value_ref.reset t.values;
        List.iter reset_next_values t.children

      (*f apply_styles *)
      let apply_styles l styles t = 
        let apply_style sid_sv =
          let (sid,sv) = sid_sv in
          match find_sid_index sid t with
            None -> ()
          | Some sindex ->
             Styleable_value.Styleable_value_ref.apply l sv t.values.(sindex) 
        in
        List.iter apply_style styles

      (*f resolve_next_value *)
      let rec resolve_next_value sid t = 
        let value_ref =
          match find_sid_index sid t with
            None -> (
            if (Stylesheet.is_default_inherit sid t.sheet) then
              Styleable_value.Styleable_value_ref.Inherit
            else
              Styleable_value.Styleable_value_ref.Default
          )
          | Some sindex ->
             (
               Styleable_value.Styleable_value_ref.next_value_ref t.values.(sindex)
             )
        in
        match value_ref with
          Styleable_value.Styleable_value_ref.Ref v -> v
        | Styleable_value.Styleable_value_ref.Default -> Stylesheet.get_default_value sid t.sheet
        | Styleable_value.Styleable_value_ref.Inherit ->
           (
             match t.parent with
               Some p -> resolve_next_value sid p
             | None -> Stylesheet.get_default_value sid t.sheet
           )

      (*f update_current_values_from_next *)
      let rec update_current_values_from_next t =
        let update_nth_value n acc =
          let sid = get_nth_sid n t in
          let value = resolve_next_value sid t in
          if (value == Styleable_value.Styleable_value_ref.get_value t.values.(n)) then acc
          else (
            (*Printf.printf "update_nth_value %s : %s : %s \n%!" t.id_name (Style_id.str sid) (str_of_svalue value);*)
            Styleable_value.Styleable_value_ref.set_value t.values.(n) value;
            (sid,value)::acc
          )
        in
        let rec update_values l n acc = 
          if (n>=l) then
            acc
          else
            let next_acc = update_nth_value n acc in
            update_values l (n+1) next_acc
        in
        let changed_sids = update_values t.num_styles 0 [] in
        if changed_sids <> [] then
          (
            (*Printf.printf "Sids changed for this %s.%s\n" t.id_name t.type_name;*)
            t.style_change_callback changed_sids
          );
        List.iter update_current_values_from_next t.children

      (*f element_callback_matching_children *)
      let rec element_callback_matching_children selector e t callback = 
        List.iter (fun e -> if (selector e) then callback e) e.children

      (*f element_callback_all_children *)
      let element_callback_all_children _ e t callback = 
        element_callback_matching_children (fun e -> true) e t callback

      (*f element_callback_matching_subelements *)
      let rec element_callback_matching_subelements selector e t callback = 
        element_callback_matching_children selector e t callback;
        element_callback_all_children selector e t (fun e -> element_callback_matching_subelements selector e t callback);
        ()

      (*f element_callback_all_subelements *)
      let element_callback_all_subelements _ e t callback = 
        element_callback_matching_children (fun e -> true) e t callback

      (*f element_callback_matching_tree *)
      let element_callback_matching_tree element_style_selector e t callback = 
        if (element_style_selector e) then (callback e);
        element_callback_matching_subelements element_style_selector e t callback

      (*f All done *)
    end
       (*m Style_rule - immutable *)
       and Style_rule : sig 
         (*m sig *)
         (*t type *)
         type t = {
             selectors : Styleable.t_style_selector list;
             styles : (Style_id.t * t_styleable_value) list;
           }
         val create : Styleable.t_style_selector list -> (Style_id.t * t_styleable_value) list -> t
         val apply : t -> Stylesheet.t -> unit
       end = struct
         (*m struct *)
         (*t type *)
         type t = {
             selectors : Styleable.t_style_selector list;
             styles : (Style_id.t * t_styleable_value) list;
           }

         (*f create *)
         let create selectors styles =
           {
             selectors;
             styles;
           }

         (*f apply *)
         let apply t stylesheet =
           let apply_style e =
             Styleable.apply_styles (List.length t.selectors) t.styles e
           in
           let rec sel_cbk_for_remaining_rules rules =
             match rules with
             | hd::nxt::tail -> 
                let (sel,cbk) = sel_cbk_for_remaining_rules (nxt::tail) in
                (hd,  fun e -> Styleable.element_callback_matching_subelements sel e stylesheet cbk)
             | hd::tail -> (hd,apply_style)
             | [] -> ((fun e -> true),apply_style)
           in
           let (sel,cbk) = sel_cbk_for_remaining_rules t.selectors in
           Stylesheet.element_callback_matching_tree stylesheet sel cbk

       (*f All done *)
       end
       (*m Stylesheet - rules, entities mutable; rest immutable *)
       and Stylesheet : sig 
         (*m sig *)
         (*t type *)
         type t = {
             mutable entity_list : Styleable.t list;
             mutable roots : Styleable.t list;
             ids : Style_ids.t;
             default_style : Style.t;
             mutable rules : Style_rule.t list;
             mutable built_descs : (Styleable_desc.t * Styleable_desc_built.t) list;
           }
         val create : unit -> t
         val build  : t -> Styleable.t list -> t
         val add_styleable : Styleable.t -> t -> unit
         val add_style_defaults : t -> (string * t_styleable_value * bool) list -> unit
         val element_callback_matching_tree : t -> Styleable.t_style_selector -> Styleable.t_style_change_fn -> unit
         val add_style_rule : t -> Styleable.t_style_selector list -> (t_styleable_name * t_styleable_value) list -> unit
         val apply_stylesheet : t -> unit
         val get_default_value  :  Style_id.t -> t -> t_styleable_value
         val is_default_inherit :  Style_id.t -> t -> bool
         val style_id_of_name     : string -> t -> Style_id.t option
         val style_id_of_name_exn : string -> t -> Style_id.t
         val build_desc : Styleable_desc.t -> t -> Styleable_desc_built.t
       end = struct
         (*m struct *)
         (*t type *)
         type t = {
             mutable entity_list : Styleable.t  list;
             mutable roots : Styleable.t list;
             ids : Style_ids.t;
             default_style : Style.t;
             mutable rules : Style_rule.t list;
             mutable built_descs : (Styleable_desc.t * Styleable_desc_built.t) list;
           }

         (*f create *)
         let create () = {
             entity_list = [];
             roots = [];
             ids = Style_ids.create ();
             default_style = Style.create [];
             rules = [];
             built_descs = [];
           }

         (*f build_desc *)
         let build_desc desc t =
           if (not (List.mem_assoc desc t.built_descs)) then
             (t.built_descs <- (desc,Styleable_desc_built.create desc t.ids)::t.built_descs);
           List.assoc desc t.built_descs

         (*f build *)
         let build t roots =
           t.roots <- roots;
           t

         (*f add_styleable *)
         let add_styleable s t =
           t.entity_list <- s :: t.entity_list

         (*f style_id_of_name *)
         let style_id_of_name name t =
           let hash = Style_id_hash.of_string name in
           Style_ids.find_opt_id hash t.ids

         (*f style_id_of_name_exn *)
         let style_id_of_name_exn name t =
           let hash = Style_id_hash.of_string name in
           Style_ids.find_id_exn hash t.ids

         (*f add_style_defaults *)
         let add_style_defaults t nvis =
           let add_id_and_style n_v_i = 
             let (name,svalue,def_inherit)=n_v_i in
             let stype = Styleable_value.stype_of_svalue svalue in
             let sid = Style_id.create name stype in
             Style_ids.add_id (Style_id_hash.of_string name) sid t.ids;
             Style.add_styling sid svalue def_inherit t.default_style
           in
           List.iter add_id_and_style nvis;
           Printf.printf "Default %s\n" (Style.str t.default_style);
           ()

         (*f get_default_value *)
         let get_default_value sid t =
           Style.get_value sid t.default_style

         (*f is_default_inherit *)
         let is_default_inherit sid t =
           Style.get_opt sid t.default_style

         (*f add_style_rule *)
         let add_style_rule t selectors style_nvs =
           let id_vs = Style_ids.build_id_value_list style_nvs t.ids in
           let rule = Style_rule.create selectors id_vs in
           t.rules <- rule :: t.rules

         (*f apply_stylesheet *)
         let apply_stylesheet t =
           (* clear new values, setting all to inherit, and clear longest matching rule
            *)
           List.iter (fun e -> Styleable.reset_next_values e) t.roots;

           (* Apply rules *)
           List.iter (fun r -> Style_rule.apply r t) t.rules;

           (* resolve next values to current values, determining which ones have changed,
    and callback if any have for each styleable
            *)
           List.iter (fun e -> Styleable.update_current_values_from_next e) t.roots;
           ()

         (*f element_callback_matching_tree *)
         let element_callback_matching_tree t selector callback =
           List.iter (fun e -> Styleable.element_callback_matching_tree selector e t callback) t.roots

       (*f All done *)
       end

       type t_stylesheet  = Stylesheet.t
       type t_styleable   = Styleable.t

       let create_desc         = Styleable_desc.create

       let se_create              = Styleable.create
       let se_get_value_ref       = Styleable.get_value_ref
       let se_get_value           = Styleable.get_value
       let se_set_element_state   = Styleable.set_element_state
       let se_set_parent          = Styleable.set_parent
       let se_is_element_id       = Styleable.is_element_id
       let se_is_element_state    = Styleable.is_element_state
       let se_is_element_type     = Styleable.is_element_type
       let se_has_element_class   = Styleable.has_element_class

       let build               = Stylesheet.build
       let apply               = Stylesheet.apply_stylesheet
       let create              = Stylesheet.create
       let add_style_defaults  = Stylesheet.add_style_defaults
       let add_style_rule      = Stylesheet.add_style_rule

end
