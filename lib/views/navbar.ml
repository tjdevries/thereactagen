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

let home_nav (user : Models.User.t) =
  (*  TODO: This is not a high enough z index or something... don't konw why the graph covers the popup *)
  let profile_url =
    match user.twitch_profile_url with
    | Some url -> url
    | None -> ""
  in
  nav
    ~a:[ a_class [ "navbar bg-base-100" ] ]
    [ div ~a:[ a_class [ "flex-1" ] ] [ txt "left button thing" ]
    ; div
        ~a:[ a_class [ "flex-none gap-2" ] ]
        [ div
            ~a:[ a_class [ "dropdown dropdown-end" ] ]
            [ label
                ~a:
                  [ a_tabindex 0
                  ; a_class [ "btn btn-ghost btn-circle avatar" ]
                  ]
                [ span
                    ~a:[ a_class [ "w-8 h-8 rounded-full" ] ]
                    [ img ~src:profile_url ~alt:"Hello" () ]
                ]
            ; ul
                ~a:
                  [ a_tabindex 0
                  ; a_class
                      [ "mt-3 z-[1] p-2 shadow menu menu-sm dropdown-content"
                      ; "bg-base-100 rounded-box w-52"
                      ]
                  ]
                [ li
                    [ a ~a:[ a_class [ "justify-between" ] ] [ txt "Profile" ] ]
                ]
            ]
        ]
    ]
;;
