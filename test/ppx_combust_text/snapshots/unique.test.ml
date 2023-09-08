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
          [ "suggestion_id"; "user_id" ]
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
              [ "suggestion_id"; "user_id" ]
          ]
        (let open Schema in
         [ field
             "id"
             ~ty:Type.int
             ~constraints:[ primary_key ~auto_increment:true (); not_null () ]
         ; field
             "user_id"
             ~ty:Type.int
             ~constraints:
               [ not_null ()
               ; foreign_key
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

    let find_one ?(select = fields) ~where ?(decode = decode) db =
      Query.select select ~from:table
      |> Query.where where
      |> Request.make_zero_or_one
      |> Petrol.find_opt db
      |> Lwt_result.map (Option.map ~f:decode)
    ;;

    let _ = find_one

    let find_many ?(select = fields) ~where ?(decode = decode) db =
      Query.select select ~from:table
      |> Query.where where
      |> Request.make_many
      |> Petrol.collect_list db
      |> Lwt_result.map (List.map ~f:decode)
    ;;

    let _ = find_many
  end
end [@@ocaml.doc "@inline"] [@@merlin.hide]
