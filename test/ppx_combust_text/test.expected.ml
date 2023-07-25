type t =
  { id : int [@primary]
  ; username : string [@unique]
  ; display_name : string
  ; password : string
  }
[@@deriving combust ~name:"users"]

include struct
  let _ = fun (_ : t) -> ()

  include struct
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"users"
        (let open Schema in
         [ field "id" ~ty:Type.int
         ; field "username" ~ty:Type.text
         ; field "display_name" ~ty:Type.text
         ; field "password" ~ty:Type.text
         ])
    ;;

    let _ = table
    and _ = fields

    let Expr.[ f_id; f_username; f_display_name; f_password ] = fields

    let _ = f_id
    and _ = f_username
    and _ = f_display_name
    and _ = f_password
  end
end [@@ocaml.doc "@inline"] [@@merlin.hide]
