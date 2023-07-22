open Base

let () = Fmt.pr "Loading...@."

let default_header title_txt =
  let open Tyxml.Html in
  head
    (title @@ txt title_txt)
    Reactagen.Scripts.[ htmx; tailwind; daisy; mystyle ]
;;

let index request =
  let open Lwt_result.Syntax in
  let* suggestions = Models.Suggestion.show_all request in
  let open Tyxml.Html in
  html
    ~a:[ Unsafe.string_attrib "data-theme" "cupcake" ]
    (default_header "TheReactagen")
    (body [ suggestions; div [ Models.Suggestion.post_form request ] ])
  |> Lwt.return_ok
;;

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt
let db_uri = "sqlite3:/home/tjdevries/tmp/reactagen.sqlite"

let () =
  let _ = Based.initialize db_uri in
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
           | Ok text -> text |> elt_to_string |> Dream.html
           | _ -> assert false)
       ; Dream.get "/static/**" (Dream.static "static/")
       ; Dream_livereload.route ()
       ]
;;
