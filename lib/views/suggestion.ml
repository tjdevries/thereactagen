open Lwt_result.Syntax

let view ~user_id ~suggestion_id request =
  let upvote_id = "suggestion-upvote" in
  let _ = user_id in
  Fmt.pr "Searching for suggestion ID: %d@." suggestion_id;
  let* suggestion =
    Dream.sql request @@ Models.Suggestion.find ~id:suggestion_id
  in
  Fmt.pr "Found suggestion@.";
  let* count = Dream.sql request @@ Models.Vote.get_vote_total ~suggestion_id in
  Fmt.pr "Found count: %d@." count;
  match suggestion with
  | Some suggestion ->
    Fmt.pr "Found suggestion ID: %d@." suggestion.id;
    let open Tyxml.Html in
    html
      (Reactagen.Header.default_header "Example Post")
      (body
         [ div
             [ div [ txt suggestion.url ]
             ; div [ txt suggestion.description ]
             ; div
                 [ txt
                     (suggestion.category
                      |> Models.Suggestion.category_to_string)
                 ]
             ; div
                 [ txt "Upvotes: "
                 ; span ~a:[ a_id upvote_id ] [ txt (Fmt.str "%d" count) ]
                 ]
             ; div
                 [ button
                     ~a:
                       [ Hx.post
                           ("/suggestion/upvote/" ^ Int.to_string suggestion_id)
                       ; Hx.target (Css ("#" ^ upvote_id))
                       ]
                     [ txt "Upvote" ]
                 ]
             ; div
                 [ button
                     ~a:
                       [ Hx.post
                           ("/suggestion/downvote/"
                            ^ Int.to_string suggestion_id)
                       ; Hx.target (Css ("#" ^ upvote_id))
                       ]
                     [ txt "Downvote" ]
                 ]
             ]
         ])
    |> Lwt.return_ok
  | None -> Fmt.failwith "Failed to find suggestion: %d" suggestion_id
;;
