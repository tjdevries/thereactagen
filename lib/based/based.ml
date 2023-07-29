let schema = Models.Schema.schema

let initialize uri =
  let open Lwt_result.Syntax in
  let connection = Uri.of_string uri in
  let* connection = Caqti_lwt.connect connection in
  let* _ = Petrol.StaticSchema.initialise schema connection in
  let* user_id =
    Models.User.create
      ~display_name:"TJ DeVries"
      ~username:"tjdevries"
      ~password:"selfpromo"
      connection
  in
  let* prime_id =
    Models.User.create
      ~display_name:"The React Agen"
      ~username:"theprimeagen"
      ~password:"thankstj"
      connection
  in
  let* suggestion_id =
    Models.Suggestion.create
      ~user_id
      ~title:"Teej's Site"
      ~url:"https://teej.tv"
      ~description:"a very cool site"
      ~category:Article
      ~status:Submitted
      connection
  in
  let* _ =
    Models.Suggestion.create
      ~user_id
      ~title:"KEKW joke stream"
      ~url:"https://twitch.tv/trash_dev"
      ~description:"No need to subscribe"
      ~category:Website
      ~status:Submitted
      connection
  in
  let* _ =
    Models.Suggestion.create
      ~user_id
      ~title:"this guy doesn't stream"
      ~url:"https://twitch.tv/adamdotdev"
      ~description:"Wish he would start again"
      ~category:Website
      ~status:Pending
      connection
  in
  let* _ =
    Models.Suggestion.create
      ~user_id
      ~title:"half of his podcast is good"
      ~url:"https://twitch.tv/thdxr"
      ~description:"tomorrow.fm"
      ~category:Website
      ~status:Complete
      connection
  in
  let* _ = Models.Vote.create ~suggestion_id ~user_id ~vote:1 connection in
  let* _ =
    Models.Vote.create ~suggestion_id ~user_id:prime_id ~vote:1 connection
  in
  let* counted = Models.Vote.get_vote_total ~suggestion_id connection in
  print_endline ("[reactagen] initialize done: " ^ Int.to_string counted);
  Lwt.return_ok true
;;
