open Base
open Lwt_result.Syntax

let make request (auth : Auth.valid_user) =
  let suggestion_form = Models.Suggestion.filter_form request in
  let* suggestions = Models.Suggestion.show_all request in
  let open Tyxml.Html in
  Reactagen.Header.html
    "𝕏 TheReactagen 𝕏"
    [ Navbar.home_nav auth.user
    ; div
        ~a:[ a_class [ "flex justify-center flex-col max-w-md mx-auto" ] ]
        [ suggestions ]
    ; div
        ~a:[ a_class [ "flex justify-center max-w-md mx-auto gap-4" ] ]
        [ suggestion_form; Models.Suggestion.post_form request ]
    ]
  |> Lwt.return_ok
;;
