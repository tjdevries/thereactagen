let schema = Models.Schema.schema

let initialize uri =
  let open Lwt_result.Syntax in
  let connection = Uri.of_string uri in
  let* connection = Caqti_lwt.connect connection in
  let* _ = Petrol.StaticSchema.initialise schema connection in
  let* user_id =
    Models.User.insert
      ~display_name:"TJ DeVries"
      ~username:"tjdevries"
      ~password:"selfpromo"
      connection
  in
  let* _ =
    Models.User.insert
      ~display_name:"The React Agen"
      ~username:"theprimeagen"
      ~password:"thankstj"
      connection
  in
  let* _ =
    Models.Suggestion.insert
      ~user_id
      ~url:"https://teej.tv"
      ~description:"a very cool site"
      ~category:Article
      connection
  in
  print_endline "[reactagen] initialize done";
  Lwt.return_ok true
;;
