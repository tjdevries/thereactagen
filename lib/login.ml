let default_header = Header.default_header

let login request =
  let redirect = Dream.query request "redirect" in
  let open Tyxml.Html in
  let make_input ~name ~text ~input_type =
    div
      [ label ~a:[ a_label_for name ] [ txt text ]
      ; input ~a:[ a_id name; a_name name; a_input_type input_type ] ()
      ]
  in
  html
    (default_header "Login")
    (body
       [ h1 [ txt "Login here" ]
       ; form
           ~a:
             [ (a_action
                @@ "/login"
                ^
                match redirect with
                | Some redirect -> "?redirect=/" ^ redirect
                | None -> "")
             ; a_method
                 `Post (* ; a_content "application/x-www-form-urlencoded" *)
             ]
           (* [ input *)
           (*     ~a: *)
           (*       [ a_name "dream.csrf" *)
           (*       ; a_input_type `Hidden *)
           (*       ; a_value (Dream.csrf_token request) *)
           (*       ] *)
           (*     () *)
           [ Dream.csrf_tag request |> Unsafe.data
           ; div
               ~a:[ a_id "twitchchat" ]
               [ make_input ~name:"name" ~text:"Username:" ~input_type:`Text
               ; make_input
                   ~name:"password"
                   ~text:"Password:"
                   ~input_type:`Password
               ; button ~a:[ a_button_type `Submit ] [ txt "Submit" ]
               ]
           ]
       ])
;;
