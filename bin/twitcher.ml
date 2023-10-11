[@@@warning "-32-26-27"]

open Lwt.Syntax

(* open Lwt *)
open Cohttp
open Cohttp_lwt_unix

let () = print_endline "==== twitch ===="

open Twitch

type unit_clients =
  { total : int
  ; data : Twitch.client list
  }
[@@deriving show, yojson { strict = false }]

type unit_users =
  { total : int
  ; data : Twitch.user list
  }
[@@deriving show, yojson { strict = false }]

type broadcaster_subscription_response = { data : Subscriber.t list }
[@@deriving show, yojson { strict = false }]

module type TWITCH_REQUEST = sig
  type request [@@deriving show, yojson { strict = false }]
  type response [@@deriving show, yojson { strict = false }]

  val query : request -> Uri.t
end

let make_headers client auth =
  Header.of_list
    [ "Authorization", "Bearer " ^ auth.access_token
    ; "Client-Id", client.client_id
    ]
;;

module MakeUserReq (M : TWITCH_REQUEST) = struct
  include M

  let get client auth request =
    let headers = make_headers client auth in
    let uri = query request in
    let* resp, body = Client.get ~headers uri in
    let* body = Cohttp_lwt.Body.to_string body in
    let parsed = Yojson.Safe.from_string body |> M.response_of_yojson in
    match parsed with
    | Ok parsed -> parsed |> Lwt.return
    | Error err -> Fmt.failwith "Failed to decode with: %s@.%s@." body err
  ;;
end

module SubscriptionsUser = struct
  type request =
    { broadcaster_id : string
    ; user_id : string
    }
  [@@deriving show, yojson { strict = false }]

  type response = { data : Twitch.Subscriber.t list }
  [@@deriving show, yojson { strict = false }]

  let query request =
    let uri = Uri.of_string "http://localhost:42069/mock/subscriptions/user" in
    Uri.with_query'
      uri
      [ "broadcaster_id", request.broadcaster_id; "user_id", request.user_id ]
  ;;
end

module RequestSubscriptionUser = MakeUserReq (SubscriptionsUser)

module SubscriptionsBroadcaster = struct
  type request = { broadcaster_id : string }
  [@@deriving show, yojson { strict = false }]

  type response = { data : Subscriber.t list }
  [@@deriving show, yojson { strict = false }]

  let query request =
    let uri = Uri.of_string "http://localhost:42069/mock/subscriptions" in
    Uri.with_query' uri [ "broadcaster_id", request.broadcaster_id ]
  ;;
end

module RequestSubscriptionBroadcaster = MakeUserReq (SubscriptionsBroadcaster)

let get_body url =
  let open Lwt.Infix in
  Client.get (Uri.of_string url)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body
  |> Cohttp_lwt.Body.to_string
  >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body
;;

let decode req url decoder =
  let* _, body = req url in
  let* body = Cohttp_lwt.Body.to_string body in
  let body = Yojson.Safe.from_string body |> decoder in
  match body with
  | Ok clients -> clients |> Lwt.return
  | Error err -> Fmt.failwith "Failed to decode %s: %s" (Uri.to_string url) err
;;

let get_client () =
  let* clients =
    decode
      Client.get
      (Uri.of_string "http://localhost:42069/units/clients")
      unit_clients_of_yojson
  in
  List.hd clients.data |> Lwt.return
;;

let get_users client =
  decode
    Client.get
    (Uri.of_string "http://localhost:42069/units/users")
    unit_users_of_yojson
;;

let get_user client =
  let* users = get_users client in
  List.hd users.data |> Lwt.return
;;

let auth_user client (user : user) =
  let url = Uri.of_string "http://localhost:42069/auth/authorize" in
  let auth_url =
    Uri.with_query'
      url
      [ "client_id", client.client_id
      ; "client_secret", client.secret
      ; "grant_type", "user_token"
      ; "user_id", user.user_id
      ; ( "scope"
        , "user:read:email user:read:subscriptions channel:read:subscriptions" )
      ]
  in
  let* auth = decode Client.post auth_url user_auth_of_yojson in
  Fmt.pr "Auth Response Status: @.%a@." pp_user_auth auth;
  auth |> Lwt.return
;;

let broadcaster_subscriptions client (user : user) auth =
  let* subs =
    RequestSubscriptionBroadcaster.get
      client
      auth
      { broadcaster_id = user.user_id }
  in
  Fmt.pr "first sub: %a@." Subscriber.pp (List.hd subs.data);
  subs.data |> Lwt.return
;;

let is_subscribed client auth ~user_id ~broadcaster_id =
  let* subs =
    RequestSubscriptionUser.get client auth { user_id; broadcaster_id }
  in
  List.length subs.data > 0 |> Lwt.return
;;

let main () =
  let* client = get_client () in
  let* users = get_users client in
  let user_1 = List.nth users.data 0 in
  let user_2 = List.nth users.data 1 in
  let* broadcaster = get_user client in
  let* broadcaster_auth = auth_user client broadcaster in
  let* subs = broadcaster_subscriptions client broadcaster broadcaster_auth in
  let first_sub = List.hd subs in
  let* sub_auth =
    auth_user
      client
      { user_id = first_sub.user_id
      ; user_login = first_sub.user_login
      ; user_display_name = first_sub.user_name
      }
  in
  let* user_response =
    RequestSubscriptionUser.get
      client
      sub_auth
      { broadcaster_id = broadcaster.user_id; user_id = first_sub.user_id }
  in
  begin
    match user_response.data with
    | [] -> Fmt.pr "@.No subscribers?@."
    | [ x ] -> Fmt.pr "Subscription Response: @.%a@." Subscriber.pp x
    | _ -> assert false
  end;
  let* is_subbed =
    is_subscribed
      client
      sub_auth
      ~user_id:first_sub.user_id
      ~broadcaster_id:broadcaster.user_id
  in
  Fmt.pr "SUBBED 1: %b@." is_subbed;
  let* other_subbed =
    is_subscribed
      client
      broadcaster_auth
      ~user_id:broadcaster.user_id
      ~broadcaster_id:first_sub.user_id
  in
  Fmt.pr "SUBBED 2: %b@." other_subbed;
  () |> Lwt.return
;;

let () =
  let _ = Lwt_main.run (main ()) in
  print_endline "Done"
;;
