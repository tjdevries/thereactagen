open Base
module Ty = Reactagen.Ty

let () = Fmt.pr "Loading...@."

let get_required_env var =
  match Stdlib.Sys.getenv var with
  | "" -> Fmt.failwith "Empty $%s" var
  | value -> value
  | exception _ -> Fmt.failwith "Missing $%s" var
;;

(* Load required environment variables *)
let () = Dotenv.export () |> ignore
let db_uri = get_required_env "REACTAGEN_DB"
let twitch_client_id = get_required_env "TWITCH_CLIENT_ID"
let twitch_client_secret = get_required_env "TWITCH_CLIENT_SECRET"
let reactagen_base_url = get_required_env "TWITCH_OAUTH_REDIRECT"
let dream_secret = get_required_env "DREAM_SECRET_KEY"
let () = Fmt.pr "OAuth Redirect Base URL: '%s' @." reactagen_base_url

(** Config for managing connections to twitch. *)
let twitch_config =
  Twitch.
    { client =
        { client_id = twitch_client_id
        ; secret = twitch_client_secret
        ; name = "TheReactagen"
        }
    ; redirect = reactagen_base_url
    }
;;

let () = Fmt.pr "Successfully found all environment@."

let index request twitch_config =
  match Auth.get_cookie request with
  | Some auth -> Views.Index.make request auth
  | None -> Views.Login.make request twitch_config
;;

let post_vote ~vote request =
  let suggestion_id = Dream.param request "id" |> Int.of_string in
  let user = Auth.get_cookie request in
  let user = Option.value_exn user in
  let%lwt _ =
    Dream.sql request
    @@ Models.Vote.create ~suggestion_id ~user_id:user.user.twitch_user_id ~vote
  in
  match%lwt Dream.sql request @@ Models.Vote.get_vote_total ~suggestion_id with
  | Ok count -> Dream.response (Fmt.str "%d" count) |> Lwt.return
  | _ -> assert false
;;

let ensure_database () =
  Lwt_main.run
  @@ match%lwt Based.initialize db_uri with
     | Ok _ -> true |> Lwt.return
     | Error err ->
       Fmt.pr "Database Errored: %a @." Caqti_error.pp err;
       false |> Lwt.return
;;

let () =
  let initialized = ensure_database () in
  Fmt.pr "[reactagen] starting dream - %b@." initialized;
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ Dream.set_secret dream_secret
  @@ Dream.cookie_sessions
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool db_uri
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           match%lwt index request twitch_config with
           | Ok page -> Ty.html_to_string page |> Dream.html
           | Error err ->
             Fmt.failwith "Failed to get index %a" Caqti_error.pp err)
       ; Dream.get "/twitch" (fun request ->
           let%lwt _ = Auth.handle_redirect request twitch_config in
           Dream.redirect request "/")
       ; Dream.get "/unauth" (fun request ->
           let html =
             Reactagen.Header.html
               "𝕏 TheReactagen 𝕏"
               [ Tyxml_html.txt "You've logged out!" ]
             |> Ty.html_to_string
           in
           let%lwt response = Dream.html html in
           Auth.drop_cookie response request)
       ; Dream.post Models.Suggestion.base_url (fun request ->
           let user = Auth.get_cookie request |> Option.value_exn in
           match%lwt
             Models.Suggestion.post_router
               ~user_id:user.user.twitch_user_id
               request
           with
           | Ok id ->
             (match%lwt Models.Suggestion.show_one request id with
              | Ok text ->
                text |> Ty.elt_to_string |> Dream.response |> Lwt.return
              | _ -> assert false)
           | _ -> assert false)
       ; Dream.get "/suggestion/user/name/:name" (fun request ->
           let twitch_display_name = Dream.param request "name" in
           match%lwt
             Dream.sql request
             @@ Models.Suggestion.find_by_twitch_username ~twitch_display_name
           with
           | Ok suggestions ->
             let x = Models.Suggestion.display_suggestions suggestions in
             let x = Reactagen.Header.html "User Stuff" [ x ] in
             x |> Ty.html_to_string |> Dream.response |> Lwt.return
           | Error _ -> assert false)
       ; Dream.post "/suggestion/table/view" (fun request ->
           match%lwt Models.Suggestion.show_all request with
           | Ok res -> res |> Ty.elt_to_string |> Dream.response |> Lwt.return
           | _ -> assert false)
       ; Dream.get "/suggestion/:id" (fun request ->
           let id = Dream.param request "id" in
           let id = Int.of_string id in
           match%lwt Models.Suggestion.show_one request id with
           | Ok text -> text |> Ty.elt_to_string |> Dream.response |> Lwt.return
           | _ -> assert false)
       ; Dream.get "/suggestion/view/:id" (fun request ->
           let id = Dream.param request "id" |> Int.of_string in
           match%lwt Views.Suggestion.view ~suggestion_id:id request with
           | Ok response -> Ty.html_to_string response |> Dream.html
           | Error err -> Fmt.failwith "%a" Caqti_error.pp err)
       ; Dream.post "/suggestion/upvote/:id" (post_vote ~vote:1)
       ; Dream.post "/suggestion/downvote/:id" (post_vote ~vote:(-1))
       ; Dream.get "/static/**" (Dream.static "static/")
       ; Dream_livereload.route ()
       ]
;;
