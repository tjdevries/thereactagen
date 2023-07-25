open Ppxlib
open Ast_helper

(* open Ast_builder.Default *)
open Base

let make_str_const s loc = Exp.constant (Pconst_string (s, loc, None))
let make_type_var txt ~loc = Exp.ident { txt; loc }
let make_pattern name loc = Ast_helper.Pat.var { txt = name; loc }
let field_prefix = "f_"

let make_pat_list labels loc =
  let patterns =
    List.map labels ~f:(fun lbl ->
      let field_name = field_prefix ^ lbl.pld_name.txt in
      make_pattern field_name loc)
  in
  let dotter = { txt = Lident "::"; loc } in
  let cons_name = Ast_helper.Pat.construct { txt = Lident "[]"; loc } None in
  List.fold_right
    patterns
    ~f:(fun item acc ->
      Ast_helper.Pat.construct
        dotter
        (Some (Ast_helper.Pat.tuple [ item; acc ])))
    ~init:cons_name
;;

(* let id_name = make_pattern "f_id" loc in *)
(* let username_name = make_pattern "f_username" loc in *)
(* let user_cons = *)
(*   Ast_helper.Pat.construct *)
(*     dotter *)
(*     (Some (Ast_helper.Pat.tuple [ username_name; cons_name ])) *)
(* in *)
(* (* Ast_helper.Pat.construct *) *)
(* Ast_helper.Pat.construct *)
(*   dotter *)
(*   (Some (Ast_helper.Pat.tuple [ id_name; user_cons ])) *)

(* Ast_helper.Pat.open_ x *)

(* Ast_helper.Pat.array [ make_pattern "hello" loc; make_pattern "world" loc ] *)
(* Ast_helper.Pat.constant *)

(* Ast_helper.Pat.open_ *)

(* Ast_helper.Pat.unpack  *)

(* let something loc = Ast_helper.Pat.unpack *)
(* let x = Ppat_unpack *)

(* let y loc = Ppat_var { txt = "hello"; loc } *)
(* let x loc = make_type_var (Longident.Ldot (Longident.Lident "Type", "int")) loc *)

let make_field_from_label loc lbl =
  let field_name = make_str_const lbl.pld_name.txt loc in
  let field_type =
    match lbl.pld_type.ptyp_desc with
    | Ptyp_constr ({ txt = Lident li; _ }, _) -> li
    | _ -> assert false
  in
  let sql_type_string =
    match field_type with
    | "int" -> "int"
    | "string" -> "text"
    | _ -> assert false
  in
  let type_def =
    make_type_var ~loc Longident.(Ldot (Lident "Type", sql_type_string))
  in
  [%expr field [%e field_name] ~ty:[%e type_def]]
;;

let generate_impl ~ctxt (_rec_flag, type_decls) name =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_decls ~f:(function
    | _ when Option.is_none name ->
      Location.raise_errorf ~loc "Missing table name"
    | { ptype_kind = Ptype_record labels; ptype_manifest; ptype_name; _ } ->
      let name = Option.value_exn name in
      let name = make_str_const name loc in
      let schema_fields = List.map labels ~f:(make_field_from_label loc) in
      let schema_fields = Ast_builder.Default.elist ~loc schema_fields in
      let pat_name = make_pat_list labels loc in
      let names = [%pat? [%p pat_name]] in
      let field_destructure = [%stri let Expr.([%p names]) = fields] in
      [%stri
        include struct
          open Petrol
          open Petrol.Sqlite3

          let table, fields =
            StaticSchema.declare_table
              schema
              ~name:[%e name]
              Schema.([%e schema_fields])
          ;;

          (* let [%e field_names] = fields *)
          [%%i field_destructure]
        end]
      (* Location.raise_errorf ~loc "not yet implemented" *)
    | _ -> assert false)
;;

let args =
  let open Deriving.Args in
  empty +> arg "name" (estring __)
;;

let impl_generator = Deriving.Generator.V2.make args generate_impl
let combust = Deriving.add "combust" ~str_type_decl:impl_generator
