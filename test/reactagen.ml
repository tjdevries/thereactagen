open Lwt_result.Syntax

let schema = Models.Schema.schema

(* Main function *)
let main f =
  let result =
    Lwt_main.run
    @@
    let dburi = Uri.of_string "sqlite3::memory:" in
    let* db = Caqti_lwt.connect dburi in
    let* () = Petrol.StaticSchema.initialise schema db in
    f db
  in
  match result with
  | Ok result -> result
  | Error err -> Fmt.failwith "Failed to execute f: %a" Caqti_error.pp err
;;

let user_1 = "1"
let user_1_name = "User One"
let user_2 = "2"
let user_2_name = "User Two"

let create_user_1 db =
  Models.User.create ~twitch_user_id:user_1 ~twitch_display_name:user_1_name db
;;

let create_user_2 db =
  Models.User.create ~twitch_user_id:user_2 ~twitch_display_name:user_2_name db
;;

let test_create_user () =
  let result = main @@ create_user_1 in
  Alcotest.(check string) "Create user" user_1 result
;;

let test_reads_user () =
  let result =
    main
    @@ fun db ->
    let* _ = create_user_1 db in
    Models.User.find_by_display_name ~twitch_display_name:user_1_name db
  in
  match result with
  | Some user ->
    Alcotest.(check string) "display_name" user_1_name user.twitch_display_name;
    Alcotest.(check string) "user_id" user_1 user.twitch_user_id
  | None -> Alcotest.fail "User not found"
;;

let test_votes_coalesce () =
  let result =
    main @@ fun db -> Models.Vote.get_vote_total ~suggestion_id:0 db
  in
  Alcotest.(check int) "Empty votes" 0 result
;;

module VoteTest = struct
  let stage_1 db =
    let* _ = create_user_1 db in
    let* _ = create_user_2 db in
    let* suggestion_id =
      Models.Suggestion.create
        ~user_id:user_1
        ~title:"one"
        ~url:"link.one"
        ~description:""
        ~category:Video
        db
    in
    let* _ = Models.Vote.create ~suggestion_id ~user_id:user_1 ~vote:1 db in
    let* _ = Models.Vote.create ~suggestion_id ~user_id:user_2 ~vote:1 db in
    Lwt_result.return suggestion_id
  ;;

  let test_vote_serialization () =
    let result =
      main
      @@ fun db ->
      let* suggestion_id = stage_1 db in
      Models.Suggestion.read suggestion_id db
    in
    match result with
    | Some suggestion ->
      if suggestion.category <> Models.Types.Category.Video
      then Alcotest.fail "Not the same category"
    | None -> Alcotest.fail "Suggestion not found"
  ;;

  let test_votes_work () =
    let result =
      main
      @@ fun db ->
      let* suggestion_id = stage_1 db in
      Models.Vote.get_vote_total ~suggestion_id db
    in
    Alcotest.(check int) "Sums two votes" 2 result
  ;;

  let test_update_votes () =
    let result =
      main
      @@ fun db ->
      let* suggestion_id = stage_1 db in
      (* Change the first vote to a downvote *)
      let* _ =
        Models.Vote.create ~suggestion_id ~user_id:user_1 ~vote:(-1) db
      in
      Models.Vote.get_vote_total ~suggestion_id db
    in
    Alcotest.(check int) "Updates vote count" 0 result
  ;;

  let test_table_join () =
    let suggestion_id, result =
      main
      @@ fun db ->
      let* suggestion_id = stage_1 db in
      let* suggestions = Models.Suggestion.suggestions_with_names db in
      (suggestion_id, suggestions) |> Lwt_result.return
    in
    let result = List.hd result in
    Alcotest.(check string) "Name" user_1_name result.twitch_display_name;
    Alcotest.(check int) "ID" suggestion_id result.suggestion.id
  ;;
end

(* Run the Lwt main loop *)
let _ =
  let open Alcotest in
  run
    "Models"
    [ ( "simple"
      , [ test_case "Insert a user: initial" `Quick test_create_user
        ; test_case "Insert a user: confirm separation" `Quick test_create_user
        ; test_case "Reads a user" `Quick test_reads_user
        ; test_case "Reads empty votes" `Quick test_votes_coalesce
        ; test_case "Counts votes" `Quick VoteTest.test_votes_work
        ; test_case "Updates votes" `Quick VoteTest.test_update_votes
        ; test_case "Serializes types" `Quick VoteTest.test_vote_serialization
        ; test_case "Joins tables" `Quick VoteTest.test_table_join
        ] )
    ]
;;
