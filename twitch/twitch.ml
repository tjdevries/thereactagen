type client =
  { client_id : string [@key "ID"]
  ; secret : string [@key "Secret"]
  ; name : string [@key "Name"]
  }
[@@deriving show, yojson { strict = false }]

type twitch_config =
  { client : client
  ; redirect : string
  }
[@@deriving show]

module Oauth = struct
  let make_redirect config state =
    Uri.with_query'
      (Uri.of_string "https://id.twitch.tv/oauth2/authorize")
      [ "response_type", "code"
      ; "client_id", config.client.client_id
      ; "redirect_uri", config.redirect
      ; "scope", "user:read:subscription"
      ; "state", state
      ]
  ;;
end

type user =
  { user_id : string [@key "id"]
  ; login : string
  ; display_name : string
  }
[@@deriving show, yojson { strict = false }]

(* Does this also have login? That would be convenient*)
type user_auth =
  { access_token : string
  ; refresh_token : string
  ; expires_in : int
  ; scope : string list
  ; token_type : string
  }
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
