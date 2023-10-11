open Base

type client =
  { client_id : string [@key "ID"]
  ; secret : string [@key "Secret"]
  ; name : string [@key "Name"]
  }

and twitch_user =
  { id : string
  ; login : string
  ; display_name : string
  ; profile_image_url : string
  }

and user =
  { user_id : string [@key "id"]
  ; user_login : string [@key "login"]
  ; user_display_name : string [@key "display_name"]
  }

and user_auth =
  { access_token : string
  ; refresh_token : string
  ; expires_in : int
  ; scope : string list
  ; token_type : string
  }

and twitch_user_request = { data : twitch_user list }
[@@deriving show, yojson { strict = false }]

(* {
  "client_id": "wbmytr93xzw8zbg0p1izqyzzc5mbiz",
  "login": "twitchdev",
  "scopes": [
    "channel:read:subscriptions"
  ],
  "user_id": "141981764",
  "expires_in": 5520838
} *)
type user_validate =
  { validated_login : string [@key "login"]
  ; validated_client : string [@key "client_id"]
  ; validated_user : string [@key "user_id"]
  }
[@@deriving show, yojson { strict = false }]

type subscription =
  { broadcaster_id : string
  ; broadcaster_login : string
  ; broadcaster_name : string
  ; gifter_id : string option [@default None]
  ; gifter_login : string option [@default None]
  ; is_gift : bool
  ; tier : string
  ; user_id : string
  ; user_name : string
  ; user_login : string
  }
[@@deriving show, yojson { strict = false }]

type twitch_config =
  { client : client
  ; redirect : string
  }
[@@deriving show]

module Oauth = struct
  (* Example URL:

     https://id.twitch.tv/oauth2/authorize
     ?response_type=code
     &client_id=hof5gwx0su6owfnys0nyan9c87zr6t
     &redirect_uri=http://localhost:3000
     &scope=channel%3Amanage%3Apolls+channel%3Aread%3Apolls
     &state=c3ab8aa609ea11e793ae92361f002671 *)
  let make_redirect ?state config =
    let state =
      Option.value_map state ~default:[] ~f:(fun s -> [ "state", s ])
    in
    let args =
      [ "response_type", "code"
      ; "client_id", config.client.client_id
      ; "redirect_uri", config.redirect ^ "twitch"
      ; "scope", "user:read:subscriptions"
      ]
      @ state
    in
    Uri.with_query' (Uri.of_string "https://id.twitch.tv/oauth2/authorize") args
  ;;

  let make_post ~code config =
    let uri = Uri.of_string "https://id.twitch.tv/oauth2/token" in
    Uri.with_query'
      uri
      [ "client_id", config.client.client_id
      ; "client_secret", config.client.secret
      ; "code", code
      ; "grant_type", "authorization_code"
      ; "redirect_uri", config.redirect ^ "twitch"
      ]
  ;;
end
