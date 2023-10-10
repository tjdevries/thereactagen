let default_header = Reactagen.Header.default_header

let login _ (client : Twitch.client) base_url =
  let open Tyxml.Html in
  Reactagen.Header.html
    "TheReactagen: Login"
    [ div
        ~a:
          [ a_class
              [ "h-screen w-screen flex flex-col justify-center items-center"
              ; "gap-4"
              ]
          ]
        [ div
            ~a:[ a_class [ "text-xl font-bold" ] ]
            [ txt "Welcome to TheReactagen" ]
        ; a
            ~a:
              [ a_href
                  (Auth.get_authorize_uri
                     ~client_id:client.client_id
                     ~redirect_uri:(base_url ^ "twitch")
                     ~scope:"user:read:subscriptions"
                     ~state:"testing1234"
                   |> Uri.to_string)
              ; a_class
                  [ "btn w-1/3 text-center"
                  ; "bg-[#9147ff] hover:bg-[#772ce8]/70 text-white"
                    (* ; "bg-clip-text text-transparent" *)
                  ]
              ]
            [ txt "Login with Twitch" ]
        ]
    ]
  |> Lwt.return_ok
;;
