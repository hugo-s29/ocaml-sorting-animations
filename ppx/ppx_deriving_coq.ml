open Ppxlib
open Ast_builder.Default

let get_animation_label (s : string) : label =
  let animation_suffix = "_animation" in
  let is_alphnum c =
    (Char.code c >= 48 && Char.code c <= 57)
    (* '0'-'9' *)
    || (Char.code c >= 65 && Char.code c <= 90)
    (* 'A'-'Z' *)
    || (Char.code c >= 97 && Char.code c <= 122 (* 'a'-'z' *))
  in
  if String.for_all (fun c -> is_alphnum c || c = '_') s then
    s ^ animation_suffix
  else
    "_" ^ string_of_int (Hashtbl.hash s) ^ animation_suffix


let animated_functions = ref []
let register_animated_function name = animated_functions := name :: !animated_functions
let has_animation_version name = List.mem name !animated_functions

let rec transform_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ }, Some arg) ->
    (match arg.pexp_desc with
     | Pexp_tuple [ hd; tl ] -> [%expr [%e transform_expr hd] ^:: [%e transform_expr tl]]
     | _ -> [%expr ( ^:: ) [%e transform_expr arg]])
  | Pexp_ident { txt = Ldot (Lident "List", fn); loc = id_loc } ->
    pexp_ident
      ~loc:id_loc
      { txt = Ldot (Ldot (Lident "Base", "List_animation"), get_animation_label fn); loc = id_loc }
  | Pexp_ident { txt = Lident name; loc = id_loc } when has_animation_version name ->
    (* Replace function name with _animation version *)
    pexp_ident ~loc:id_loc { txt = Lident (get_animation_label name); loc = id_loc }
  | Pexp_function (params, constraint_, body) ->
    let new_body =
      match body with
      | Pfunction_cases (cases, loc, attrs) ->
        let new_cases =
          List.map (fun case -> { case with pc_rhs = transform_expr case.pc_rhs }) cases
        in
        Pfunction_cases (new_cases, loc, attrs)
      | Pfunction_body body -> Pfunction_body (transform_expr body)
    in
    pexp_function ~loc params constraint_ new_body
  | Pexp_match (e, cases) ->
    pexp_match
      ~loc
      (transform_expr e)
      (List.map (fun case -> { case with pc_rhs = transform_expr case.pc_rhs }) cases)
  | Pexp_let (rec_flag, bindings, body) ->
    pexp_let
      ~loc
      rec_flag
      (List.map (fun vb -> { vb with pvb_expr = transform_expr vb.pvb_expr }) bindings)
      (transform_expr body)
  | Pexp_apply (fn, args) ->
    pexp_apply ~loc (transform_expr fn) (List.map (fun (lbl, e) -> (lbl, transform_expr e)) args)
  | Pexp_ifthenelse (cond, then_e, else_e) ->
    pexp_ifthenelse
      ~loc
      (transform_expr cond)
      (transform_expr then_e)
      (Option.map transform_expr else_e)
  | Pexp_tuple exprs -> pexp_tuple ~loc (List.map transform_expr exprs)
  | Pexp_sequence (e1, e2) -> pexp_sequence ~loc (transform_expr e1) (transform_expr e2)
  | _ -> expr


let has_animate_attribute vb =
  List.exists
    (fun attr ->
       match attr.attr_name.txt with
       | "animate" -> true
       | _ -> false)
    vb.pvb_attributes


let remove_animate_attribute vb =
  let new_attrs =
    List.filter
      (fun attr ->
         match attr.attr_name.txt with
         | "animate" -> false
         | _ -> true)
      vb.pvb_attributes
  in
  { vb with pvb_attributes = new_attrs }


let generate_animation_binding vb =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var { txt; loc } ->
    let new_name = get_animation_label txt in
    let new_pat = ppat_var ~loc:vb.pvb_pat.ppat_loc { txt = new_name; loc } in
    let new_expr = transform_expr vb.pvb_expr in
    Some { vb with pvb_pat = new_pat; pvb_expr = new_expr }
  | _ -> None


let expand_structure ~ctxt:_ str =
  (* First pass: collect all function names that will have animation versions *)
  List.iter
    (fun item ->
       match item.pstr_desc with
       | Pstr_value (_, bindings) ->
         List.iter
           (fun vb ->
              if has_animate_attribute vb then (
                match vb.pvb_pat.ppat_desc with
                | Ppat_var { txt; _ } -> register_animated_function txt
                | _ -> ()))
           bindings
       | _ -> ())
    str;
  (* Add "open Visualizer.Runtime" at the beginning *)
  let loc = Location.none in
  let open_runtime_module =
    pstr_open
      ~loc
      { popen_expr = pmod_ident ~loc { txt = Ldot (Lident "Visualizer", "Runtime"); loc };
        popen_override = Fresh;
        popen_loc = loc;
        popen_attributes = []
      }
  in
  let has_changed = ref false in
  (* Second pass: transform the structure *)
  let transformed =
    List.concat_map
      (fun item ->
         match item.pstr_desc with
         | Pstr_value (rec_flag, bindings) ->
           (* Separate bindings with and without [@animate] *)
           let bindings_to_animate = List.filter has_animate_attribute bindings in
           let cleaned_bindings = List.map remove_animate_attribute bindings in
           (* Generate animation versions only for annotated bindings *)
           let animated_bindings = List.filter_map generate_animation_binding bindings_to_animate in
           if List.length animated_bindings > 0 then (
             has_changed := true;
             [ { item with pstr_desc = Pstr_value (rec_flag, cleaned_bindings) };
               { item with pstr_desc = Pstr_value (rec_flag, animated_bindings) }
             ])
           else
             [ { item with pstr_desc = Pstr_value (rec_flag, cleaned_bindings) } ]
         | _ -> [ item ])
      str
  in
  if !has_changed then
    open_runtime_module :: transformed
  else
    transformed


let structure_rule =
  object
    inherit Ast_traverse.map as super
    method! structure str = expand_structure ~ctxt:() (super#structure str)
  end


let () = Driver.register_transformation ~impl:structure_rule#structure "sorting_animation"
