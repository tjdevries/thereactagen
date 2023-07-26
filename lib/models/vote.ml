(* open Base *)
(* open Lwt_result.Syntax *)

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type t =
  { suggestion_id : int [@foreign Suggestion.table [ Suggestion.f_id ]]
  ; user_id : int [@foreign User.table [ User.f_id ]]
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
