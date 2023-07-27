let get_user _ =
  (* let user_id = Dream.session_field request "user" in *)
  (* match user_id with *)
  (* | None -> *)
  (*   let%lwt () = Dream.invalidate_session request in *)
  (*   let%lwt () = Dream.set_session_field request "user" "1" in *)
  (*   Some 1 |> Lwt.return *)
  (* | Some _ -> Some 1 |> Lwt.return *)
  1 |> Lwt.return_ok
;;
