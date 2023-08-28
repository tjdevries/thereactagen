[@@@warning "-26-27"]

open Base
open Lwt.Syntax

(* open Cohttp *)
open Cohttp_lwt_unix

let valid_user_key = "valid_user"

type valid_user =
  { user : Models.User.t
  ; user_access : string
  ; user_refresh : string
  }
[@@deriving show, yojson { strict = false }]

(*

Step 1:

Check the session, do i have `twitch_auth` and `twitch_refresh`?
If act like logged in until taking an action (so view-only basically?)

https://id.twitch.tv/oauth2/authorize
    ?response_type=code
    &client_id=hof5gwx0su6owfnys0nyan9c87zr6t
    &redirect_uri=http://localhost:3000
    &scope=channel%3Amanage%3Apolls+channel%3Aread%3Apolls
    &state=c3ab8aa609ea11e793ae92361f002671

*)

let get_authorize_uri ~client_id ~redirect_uri ~scope ~state =
  let uri = Uri.of_string "https://id.twitch.tv/oauth2/authorize" in
  Uri.with_query'
    uri
    [ "response_type", "code"
    ; "client_id", client_id
    ; "redirect_uri", redirect_uri
    ; "scope", scope
    ; "state", state
    ]
;;

(*
  http://localhost:3000/
      ?code=gulfwdmys5lsm6qyz4xiz9q32l10
      &scope=channel%3Amanage%3Apolls+channel%3Aread%3Apolls
      &state=c3ab8aa609ea11e793ae92361f002671
*)

let send_post ~(client : Twitch.client) ~code ~redirect_uri =
  let uri = Uri.of_string "https://id.twitch.tv/oauth2/token" in
  let uri =
    Uri.with_query'
      uri
      [ "client_id", client.client_id
      ; "client_secret", client.secret
      ; "code", code
      ; "grant_type", "authorization_code"
      ; "redirect_uri", redirect_uri
      ]
  in
  let* resp, body = Client.post uri in
  let* body = Cohttp_lwt.Body.to_string body in
  let parsed = Yojson.Safe.from_string body |> Twitch.user_auth_of_yojson in
  match parsed with
  | Ok auth -> auth |> Lwt.return
  | Error err -> Fmt.failwith "Failed to parse the auth: %s@.%s" err body
;;

let set_cookie request auth =
  let encoded = valid_user_to_yojson auth |> Yojson.Safe.to_string in
  let* () = Dream.set_session_field request valid_user_key encoded in
  () |> Lwt.return
;;

(*  TODO: This isn't good :) *)
let get_cookie request =
  Dream.session_field request valid_user_key
  |> Option.find_map ~f:(fun str ->
       let x = str |> Yojson.Safe.from_string |> valid_user_of_yojson in
       match x with
       | Ok x -> Some x
       | _ -> None)
;;

let drop_cookie response request =
  let open Lwt.Syntax in
  let* () = Dream.invalidate_session request in
  Lwt.return response
;;

(* TODO: Add a refresh_token here *)
let validate_request access_token =
  let* resp, body =
    Client.get
      ~headers:
        (Cohttp.Header.of_list [ "Authorization", "OAuth " ^ access_token ])
      (Uri.of_string "https://id.twitch.tv/oauth2/validate")
  in
  (* TODO: Handle bad requests and unauthed stuff here. *)
  let _ =
    Cohttp.Response.status resp |> Cohttp.Code.code_of_status |> Int.to_string
  in
  let* body = Cohttp_lwt.Body.to_string body in
  match Yojson.Safe.from_string body |> Twitch.user_validate_of_yojson with
  | Ok validated -> validated |> Lwt.return
  | Error err -> Fmt.failwith "Could not decode user_auth: %s %s" err body
;;

let handle_redirect request client =
  let code = Dream.query request "code" in
  let scope = Dream.query request "scope" in
  let state = Dream.query request "state" in
  match code, scope, state with
  | Some code, Some scope, Some state ->
    let* user_auth =
      send_post ~client ~code ~redirect_uri:"http://localhost:8080/twitch"
    in
    let* validated = validate_request user_auth.access_token in
    let* user_model =
      Dream.sql request @@ Models.User.read validated.validated_user
    in
    let* user_model =
      match user_model with
      | Ok (Some user_model) -> user_model |> Lwt.return
      | Ok None ->
        let* _ =
          Dream.sql request
          @@ Models.User.create
               ~twitch_user_id:validated.validated_user
               ~twitch_display_name:validated.validated_login
        in
        Models.User.
          { twitch_user_id = validated.validated_user
          ; twitch_display_name = validated.validated_login
          }
        |> Lwt.return
      | _ -> Fmt.failwith "couldn't find user LUL"
    in
    let user =
      { user = user_model
      ; user_refresh = user_auth.refresh_token
      ; user_access = user_auth.access_token
      }
    in
    let _ = set_cookie request user in
    user_auth |> Lwt.return
  | _ -> assert false
;;

(*

Step 2:

To get the tokens, send an HTTP POST request to https://id.twitch.tv/oauth2/token. Set the following x-www-form-urlencoded parameters in the body of the POST.

client_id=hof5gwx0su6owfnys0yan9c87zr6t
&client_secret=41vpdji4e9gif29md0ouet6fktd2
&code=gulfwdmys5lsm6qyz4xiz9q32l10
&grant_type=authorization_code
&redirect_uri=http://localhost:3000

response:

{
  "access_token": "rfx2uswqe8l4g1mkagrvg5tv0ks3",
  "expires_in": 14124,
  "refresh_token": "5b93chm6hdve3mycz05zfzatkfdenfspp1h1ar2xxdalen01",
  "scope": [
    "channel:moderate",
    "chat:edit",
    "chat:read"
  ],
  "token_type": "bearer"
}

Dream.get /twitch-auth
-> Dream.set_session_field request "twitch_auth" access_token
-> Dream.set_session_field request "twitch_refresh" refresh_token


SOME TIME LATER

user about to submit suggestion
- Rate limit them (less than 100 actions in the last hour)
- Check https://id.twitch.tv/oauth2/validate to make sure tokens are good
  (also try refresh)
If good, then allow submit
otherwise, new session and send to auth workflow

*)
