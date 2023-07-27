open Base

let () = Fmt.pr "Loading...@."

let index request =
  let open Lwt_result.Syntax in
  let* suggestions = Models.Suggestion.show_all request in
  let open Tyxml.Html in
  Reactagen.Header.html
    "𝕏 TheReactagen 𝕏"
    [ div
        ~a:[ a_class [ "flex justify-center flex-col max-w-md mx-auto" ] ]
        [ suggestions ]
    ; div
        ~a:[ a_class [ "flex justify-center flex-col max-w-md mx-auto" ] ]
        [ Models.Suggestion.post_form request ]
    ]
  |> Lwt.return_ok
;;

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt
let db_uri = "sqlite3:/home/tjdevries/tmp/reactagen.sqlite"

let post_vote ~vote request =
  let suggestion_id = Dream.param request "id" |> Int.of_string in
  let%lwt user_id = Reactagen.Auth.get_user request in
  (* TODO: Not logged in *)
  let user_id =
    match user_id with
    | Ok user_id -> user_id
    | _ -> assert false
  in
  let _ =
    Dream.sql request @@ Models.Vote.create ~suggestion_id ~user_id ~vote
  in
  match%lwt Dream.sql request @@ Models.Vote.get_vote_total ~suggestion_id with
  | Ok count -> Dream.response (Fmt.str "%d" count) |> Lwt.return
  | _ -> assert false
;;

let () =
  let _ =
    match%lwt Based.initialize db_uri with
    | Ok _ -> true |> Lwt.return
    | Error err ->
      Fmt.pr "Database Errored: %a @." Caqti_error.pp err;
      true |> Lwt.return
  in
  Fmt.pr "[reactagen] starting dream@.";
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool db_uri
  @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           match%lwt index request with
           | Ok page -> html_to_string page |> Dream.html
           | _ -> assert false)
       ; Dream.post "/user" (fun request ->
           match%lwt Models.User.post_router request with
           | Ok id -> Dream.html @@ Fmt.str "Posted: %d" id
           | _ -> assert false)
       ; Dream.post Models.Suggestion.base_url (fun request ->
           match%lwt Models.Suggestion.post_router request with
           | Ok id ->
             (match%lwt Models.Suggestion.show_one request id with
              | Ok text -> text |> elt_to_string |> Dream.response |> Lwt.return
              | _ -> assert false)
           | _ -> assert false)
       ; Dream.get "/suggestion/:id" (fun request ->
           let id = Dream.param request "id" in
           let id = Int.of_string id in
           match%lwt Models.Suggestion.show_one request id with
           | Ok text -> text |> elt_to_string |> Dream.response |> Lwt.return
           | _ -> assert false)
       ; Dream.get "/suggestion/view/:id" (fun request ->
           let id = Dream.param request "id" |> Int.of_string in
           let%lwt user_id = Reactagen.Auth.get_user request in
           match%lwt
             Views.Suggestion.view ~user_id ~suggestion_id:id request
           with
           | Ok response -> html_to_string response |> Dream.html
           | Error err -> Fmt.failwith "%a" Caqti_error.pp err)
       ; Dream.post "/suggestion/upvote/:id" (post_vote ~vote:1)
       ; Dream.post "/suggestion/downvote/:id" (post_vote ~vote:(-1))
       ; Dream.get "/static/**" (Dream.static "static/")
       ; Dream_livereload.route ()
       ]
;;
