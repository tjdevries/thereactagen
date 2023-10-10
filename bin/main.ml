open Base
open Lwt_result.Syntax

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
let () = Fmt.pr "Successfully found all environment@."

let logged_in request (auth : Auth.valid_user) =
  let suggestion_form = Models.Suggestion.filter_form request in
  let* suggestions = Models.Suggestion.show_all request in
  let open Tyxml.Html in
  Reactagen.Header.html
    "𝕏 TheReactagen 𝕏"
    [ Views.Navbar.home_nav auth.user
    ; div
        ~a:[ a_class [ "flex justify-center flex-col max-w-md mx-auto" ] ]
        [ suggestions ]
    ; div
        ~a:[ a_class [ "flex justify-center max-w-md mx-auto gap-4" ] ]
        [ suggestion_form; Models.Suggestion.post_form request ]
    ]
  |> Lwt.return_ok
;;

let index request client =
  match Auth.get_cookie request with
  | Some auth -> logged_in request auth
  | None -> Views.Login.login request client reactagen_base_url
;;

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt

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

let () =
  (* Sleep for 1 second *)
  let client =
    Twitch.
      { client_id = twitch_client_id
      ; secret = twitch_client_secret
      ; name = "TheReactagen"
      }
  in
  let initialized =
    Lwt_main.run
    @@ match%lwt Based.initialize db_uri with
       | Ok _ -> true |> Lwt.return
       | Error err ->
         Fmt.pr "Database Errored: %a @." Caqti_error.pp err;
         true |> Lwt.return
  in
  Fmt.pr "[reactagen] starting dream - %b@." initialized;
  (* TODO: Wen deploying, we need to turn on tls,
     get a cert file and a key file, point the server to that. *)
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ Dream.set_secret dream_secret
  @@ Dream.cookie_sessions
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool db_uri
  @@ Dream.router
       [ Dream.get "/" (fun request ->
           match%lwt index request client with
           | Ok page -> html_to_string page |> Dream.html
           | Error err ->
             Fmt.failwith "Failed to get index %a" Caqti_error.pp err)
       ; Dream.get "/twitch" (fun request ->
           let%lwt _ = Auth.handle_redirect request client reactagen_base_url in
           Dream.redirect request "/")
       ; Dream.get "/unauth" (fun request ->
           let html =
             Reactagen.Header.html
               "𝕏 TheReactagen 𝕏"
               [ Tyxml_html.txt "You've logged out!" ]
             |> html_to_string
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
              | Ok text -> text |> elt_to_string |> Dream.response |> Lwt.return
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
             x |> html_to_string |> Dream.response |> Lwt.return
           | Error _ -> assert false)
       ; Dream.post "/suggestion/table/view" (fun request ->
           match%lwt Models.Suggestion.show_all request with
           | Ok res -> res |> elt_to_string |> Dream.response |> Lwt.return
           | _ -> assert false)
       ; Dream.get "/suggestion/:id" (fun request ->
           let id = Dream.param request "id" in
           let id = Int.of_string id in
           match%lwt Models.Suggestion.show_one request id with
           | Ok text -> text |> elt_to_string |> Dream.response |> Lwt.return
           | _ -> assert false)
       ; Dream.get "/suggestion/view/:id" (fun request ->
           let id = Dream.param request "id" |> Int.of_string in
           match%lwt Views.Suggestion.view ~suggestion_id:id request with
           | Ok response -> html_to_string response |> Dream.html
           | Error err -> Fmt.failwith "%a" Caqti_error.pp err)
       ; Dream.post "/suggestion/upvote/:id" (post_vote ~vote:1)
       ; Dream.post "/suggestion/downvote/:id" (post_vote ~vote:(-1))
       ; Dream.get "/static/**" (Dream.static "static/")
       ; Dream_livereload.route ()
       ]
;;
