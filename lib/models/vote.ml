open Base
(* open Lwt_result.Syntax *)

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

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

module Route = struct
  let base_url = "/suggestion/vote"
  let upvote ~suggestion_id = Fmt.str "%s/up/%d" base_url suggestion_id
  let downvote ~suggestion_id = Fmt.str "%s/down/%d" base_url suggestion_id

  let post_vote ~vote ctx request =
    let vote =
      match vote with
      | `Upvote -> 1
      | `Downvote -> -1
    in
    let suggestion_id = Dream.param request "id" |> Int.of_string in
    let user_id = ctx request in
    let%lwt _ = Dream.sql request @@ create ~suggestion_id ~user_id ~vote in
    match%lwt Dream.sql request @@ get_vote_total ~suggestion_id with
    | Ok count -> Dream.response (Fmt.str "%d" count) |> Lwt.return
    | _ -> assert false
  ;;

  (* TODO: I really don't like ctx... need some middleware step to load these up w/out cycles *)
  let scope middleware ctx =
    Dream.scope
      base_url
      middleware
      [ Dream.post "/up/:id" (post_vote ~vote:`Upvote ctx)
      ; Dream.post "/down/:id" (post_vote ~vote:`Downvote ctx)
      ]
  ;;
end
