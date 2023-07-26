type t =
  { id : int [@primary { auto_increment = true }]
  ; username : string [@unique]
  ; display_name : string
  ; password : string
  }
[@@deriving combust ~name:"users"]

include struct
  let _ = fun (_ : t) -> ()

  include struct
    open Base
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"users"
        ~constraints:[]
        (let open Schema in
         [ field
             "id"
             ~ty:Type.int
             ~constraints:[ primary_key ~auto_increment:true () ]
         ; field
             "username"
             ~ty:Type.text
             ~constraints:[ unique ~name:"username_unique" () ]
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

    let decode (id, (username, (display_name, (password, ())))) =
      { id; username; display_name; password }
    ;;

    let _ = decode

    let create ~username ~display_name ~password db =
      Query.insert
        ~table
        ~values:
          (let open Expr in
           [ f_username := s username
           ; f_display_name := s display_name
           ; f_password := s password
           ])
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

type suggestion =
  { id : int [@primary { auto_increment = true }]
  ; user_id : int [@foreign User.table [ User.f_id ]]
  }
[@@deriving
  combust
    ~name:"suggestions"
    ~constraints:
      [ Schema.table_primary_key
          ~name:"unique_suggestion_and_post"
          ~on_conflict:`REPLACE
          [ suggestion_id; user_id ]
      ]]

include struct
  let _ = fun (_ : suggestion) -> ()

  include struct
    open Base
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"suggestions"
        ~constraints:
          [ Schema.table_primary_key
              ~name:"unique_suggestion_and_post"
              ~on_conflict:`REPLACE
              [ suggestion_id; user_id ]
          ]
        (let open Schema in
         [ field
             "id"
             ~ty:Type.int
             ~constraints:[ primary_key ~auto_increment:true () ]
         ; field
             "user_id"
             ~ty:Type.int
             ~constraints:
               [ foreign_key
                   ~table:User.table
                   ~columns:
                     (let open Expr in
                      [ User.f_id ])
                   ~on_update:`RESTRICT
                   ~on_delete:`CASCADE
                   ()
               ]
         ])
    ;;

    let _ = table
    and _ = fields

    let Expr.[ f_id; f_user_id ] = fields

    let _ = f_id
    and _ = f_user_id

    let decode (id, (user_id, ())) = { id; user_id }
    let _ = decode

    let create ~user_id db =
      Query.insert
        ~table
        ~values:
          (let open Expr in
           [ f_user_id := i user_id ])
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
