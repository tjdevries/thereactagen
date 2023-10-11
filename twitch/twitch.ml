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

and user_validate =
  { validated_login : string [@key "login"]
  ; validated_client : string [@key "client_id"]
  ; validated_user : string [@key "user_id"]
  }
[@@deriving show, yojson { strict = false }]

type twitch_config =
  { client : client
  ; redirect : string
  }
[@@deriving show]

(** https://dev.twitch.tv/docs/authentication/getting-tokens-oauth/#authorization-code-grant-flow *)
module Oauth = struct
  (** Make the redirect URI for the twitch oauth flow. *)
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

  (** Make the URI for posting and retrieving the user auth token *)
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

module Subscriber = struct
  type t =
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
end
