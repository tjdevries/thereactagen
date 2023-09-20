type t =
  { suggestion_id : int [@foreign Suggestion.table [ Suggestion.f_id ]]
  ; user_id : string [@foreign User.table [ User.f_twitch_user_id ]]
  ; vote : int
  }
[@@deriving
  combust
    ~name:"votes"
    ~constraints:
      [ Schema.table_primary_key
          ~name:"unique_suggestion_and_post"
          ~on_conflict:`REPLACE
          [ "suggestion_id"; "user_id" ]
      ]]

include struct
  let _ = fun (_ : t) -> ()

  include struct
    open Base
    open Petrol
    open Petrol.Sqlite3

    let table, fields =
      StaticSchema.declare_table
        schema
        ~name:"votes"
        ~constraints:
          [ Schema.table_primary_key
              ~name:"unique_suggestion_and_post"
              ~on_conflict:`REPLACE
              [ "suggestion_id"; "user_id" ]
          ]
        (let open Schema in
         [ field
             "suggestion_id"
             ~ty:Type.int
             ~constraints:
               [ not_null ()
               ; foreign_key
                   ~table:Suggestion.table
                   ~columns:
                     (let open Expr in
                      [ Suggestion.f_id ])
                   ~on_update:`RESTRICT
                   ~on_delete:`CASCADE
                   ()
               ]
         ; field
             "user_id"
             ~ty:Type.text
             ~constraints:
               [ not_null ()
               ; foreign_key
                   ~table:User.table
                   ~columns:
                     (let open Expr in
                      [ User.f_twitch_user_id ])
                   ~on_update:`RESTRICT
                   ~on_delete:`CASCADE
                   ()
               ]
         ; field "vote" ~ty:Type.int ~constraints:[ not_null () ]
         ])
    ;;

    let _ = table
    and _ = fields

    let Expr.[ f_suggestion_id; f_user_id; f_vote ] = fields

    let _ = f_suggestion_id
    and _ = f_user_id
    and _ = f_vote

    let decode (suggestion_id, (user_id, (vote, ()))) =
      { suggestion_id; user_id; vote }
    ;;

    let _ = decode

    let create ~suggestion_id ~user_id ~vote db =
      Query.insert
        ~table
        ~values:
          (let open Expr in
           [ f_suggestion_id := i suggestion_id
           ; f_user_id := s user_id
           ; f_vote := i vote
           ])
      |> Request.make_zero
      |> Petrol.exec db
    ;;

    let _ = create
    let () = ()
    let () = ()

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
