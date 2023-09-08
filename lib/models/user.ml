open Base

let schema = Schema.schema

open Petrol.Sqlite3

(*
   Will probably forget i have this note..

   1. We should display who made a post
   2. We should add roles so that we can start doing things like a workflow
   of moving suggestions from one state to another.
   3. Need to regularly validate that this is an active OAuth token when
   about to post something. Should then try refresh if it fails. Then
   do auth workflow.

   - Subs get multiple votes (of course)
*)

type t =
  { twitch_user_id : string [@primary]
  ; twitch_display_name : string
  }
[@@deriving show, yojson, combust ~name:"users"]

let find_by_display_name ~twitch_display_name db =
  find_one ~where:Expr.(f_twitch_display_name = s twitch_display_name) db
;;

let find_data t key =
  let data =
    List.find_map t ~f:(fun (header, data) ->
      if String.(header = key) then Some data else None)
  in
  match data with
  | Some data -> Lwt.return_ok data
  | None -> Lwt.return_error (`Missing_header key)
;;

let get_form request =
  match%lwt Dream.form request with
  | `Ok form_data -> Lwt.return_ok form_data
  | _ -> Lwt.return_error `Missing_form
;;

let make_input ~name ~text ~input_type =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "mb-6" ] ]
    [ label
        ~a:
          [ a_label_for name
          ; a_class
              [ "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ]
          ]
        [ txt text ]
    ; input
        ~a:
          [ a_id name
          ; a_name name
          ; a_input_type input_type
          ; a_class
              [ "bg-gray-50 border border-gray-300 text-gray-900 text-sm \
                 rounded-lg focus:ring-blue-500 focus:border-blue-500 block \
                 w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 \
                 dark:placeholder-gray-400 dark:text-white \
                 dark:focus:ring-blue-500 dark:focus:border-blue-500"
              ]
          ]
        ()
    ]
;;

let post_form request =
  let open Tyxml.Html in
  form
    ~a:
      [ a_action "/user"
      ; a_method `Post (* ; a_content "application/x-www-form-urlencoded" *)
      ]
    [ Dream.csrf_tag request |> Unsafe.data
    ; div
        ~a:[ a_id "twitchchat" ]
        [ make_input ~name:"username" ~text:"Username:" ~input_type:`Text
        ; make_input
            ~name:"display_name"
            ~text:"Display Name:"
            ~input_type:`Text
        ; make_input ~name:"password" ~text:"Password:" ~input_type:`Password
        ; button
            ~a:[ a_button_type `Submit; Unsafe.string_attrib "disabled" "" ]
            [ txt "Submit" ]
        ]
    ]
;;
