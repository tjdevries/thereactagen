(* open Base *)
(* open Lwt_result.Syntax *)

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

let table, fields =
  let suggestion_id, user_id = "suggestion_id", "user_id" in
  StaticSchema.declare_table
    schema
    ~name:"votes"
    ~constraints:
      [ Schema.table_primary_key
          ~name:"unique_suggestion_and_post"
          ~on_conflict:`REPLACE
          [ suggestion_id; user_id ]
      ]
    Schema.
      [ field
          suggestion_id
          ~ty:Type.int
          ~constraints:
            [ foreign_key
                ~table:Suggestion.table
                ~columns:Expr.[ Suggestion.f_id ]
                ~on_update:`RESTRICT
                ~on_delete:`CASCADE
                ()
            ]
      ; field
          user_id
          ~ty:Type.int
          ~constraints:
            [ foreign_key
                ~table:User.table
                ~columns:Expr.[ User.f_id ]
                ~on_update:`RESTRICT
                ~on_delete:`CASCADE
                ()
            ]
      ; field "vote" ~ty:Type.int
      ]
;;

let Expr.[ f_suggestion_id; f_user_id; f_vote ] = fields

let insert ~suggestion_id ~user_id ~vote db =
  Query.insert
    ~table
    ~values:
      Expr.
        [ f_suggestion_id := i suggestion_id
        ; f_user_id := i user_id
        ; f_vote := i vote
        ]
  |> Request.make_zero
  |> Petrol.exec db
;;

let get_vote_total ~suggestion_id db =
  Query.select ~from:table Expr.[ coalesce [ sum f_vote; i 0 ] ]
  |> Query.where Expr.(f_suggestion_id = i suggestion_id)
  |> Request.make_zero_or_one
  |> Petrol.find_opt db
  |> Lwt_result.map (fun result ->
       match result with
       | Some (count, _) -> count
       | _ -> 0)
;;
