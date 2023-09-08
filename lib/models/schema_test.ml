let schema = Schema.schema

type t =
  { id : int [@primary]
  ; username : string option
  }

include struct
  let _ = fun (_ : t) -> ()

  include struct
    open Base
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"optional"
        ~constraints:[]
        (let open Schema in
         [ field "id" ~ty:Type.int ~constraints:[ primary_key (); not_null () ]
         ; field "username" ~ty:(Type.null_ty Type.text)
         ])
    ;;

    let _ = table
    and _ = fields

    let Expr.[ f_id; f_username ] = fields

    let _ = f_id
    and _ = f_username

    let decode (id, (username, ())) = { id; username }
    let _ = decode

    let create ~id ~username db =
      Query.insert
        ~table
        ~values:
          (let open Expr in
           [ f_id := i id; f_username := s_opt username ])
      |> Query.returning
           (let open Expr in
            [ f_id ])
      |> Request.make_one
      |> Petrol.find db
      |> Lwt_result.map fst
    ;;

    let _ = create

    let read id db =
      Query.select ~from:table fields
      |> Query.where
           (let open Expr in
            f_id = i id)
      |> Request.make_zero_or_one
      |> Petrol.find_opt db
      |> Lwt_result.map (Option.map ~f:decode)
    ;;

    let _ = read
  end
end [@@ocaml.doc "@inline"] [@@merlin.hide]
