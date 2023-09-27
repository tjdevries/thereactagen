open Tyxml_html

let nav_item link content =
  li
    [ a
        ~a:
          [ a_class [ "crumb"; "btn btn-ghost normal-case text-xl" ]
          ; a_href link
          ]
        [ content ]
    ]
;;

let nav_elt elts = nav ~a:[ a_class [ "navbar" ] ] [ ol elts ]

module Links = struct
  let home = a ~a:[ a_href "/" ] [ txt "Home" ]
end

let home_nav (user : Models.User.t) =
  (*  TODO: This is not a high enough z index or something... don't konw why the graph covers the popup *)
  let profile_url =
    match user.twitch_profile_url with
    | Some url -> url
    | None -> ""
  in
  nav
    ~a:
      [ a_class
          [ "flex items-center justify-around"
          ; "bg-secondary text-secondary-content"
          ; "px-4"
          ]
      ]
    [ div ~a:[ a_class [ "flex-1" ] ] [ Links.home ]
    ; div
        ~a:[ a_class [ "flex-none" ] ]
        [ div
            ~a:[ a_class [ "dropdown dropdown-end" ] ]
            [ label
                ~a:[ a_tabindex 0; a_class [ "m-1" ] ]
                [ span
                    ~a:[ a_class [] ]
                    [ img
                        ~a:[ a_class [ "h-8 w-8" ] ]
                        ~src:profile_url
                        ~alt:"Hello"
                        ()
                    ]
                ]
            ; ul
                ~a:
                  [ a_tabindex 0
                  ; a_class
                      [ "mt-3 z-[1] p-2 shadow-xl menu menu-sm dropdown-content"
                      ; "bg-primary-focus rounded-box w-48"
                      ; "text-accent hover:text-secondary"
                      ]
                  ]
                [ li
                    [ a ~a:[ a_class [ "justify-between" ] ] [ txt "Profile" ]
                    ; a
                        ~a:[ a_class [ "justify-between" ] ]
                        [ txt "Other thing" ]
                    ]
                ]
            ]
        ]
    ]
;;
