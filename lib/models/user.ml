open Base
open Lwt_result.Syntax

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

(** The User type  *)
type t =
  { id : int
  ; username : string
  ; display_name : string
  ; password : string
  }

let table, Expr.[ f_id; f_username; f_password; f_display_name ] =
  StaticSchema.declare_table
    schema
    ~name:"users"
    Schema.
      [ field "id" ~ty:Type.int ~constraints:[ primary_key ~auto_increment:true () ]
      ; field
          "username"
          ~ty:Type.text
          ~constraints:
            [ unique
                ~name:"username_unique"
                ~on_conflict:`FAIL (* TODO: Is this the right conflict? *)
                ()
            ]
      ; field "password" ~ty:Type.text
      ; field "display_name" ~ty:Type.text
      ]
;;

let insert ~username ~display_name ~password db =
  Query.insert
    ~table
    ~values:
      Expr.
        [ f_username := s username
        ; f_display_name := s display_name
        ; f_password := s password
        ]
  |> Query.returning Expr.[ f_id ]
  |> Request.make_one
  |> Petrol.find db
  |> Lwt_result.map fst
;;

let decode (id, (username, (display_name, (password, ())))) =
  { id; username; display_name; password }
;;

let fields = Expr.[ f_id; f_username; f_display_name; f_password ]

let find_user ~username db =
  Query.select ~from:table fields
  |> Query.where Expr.(f_username = s username)
  |> Request.make_zero_or_one
  |> Petrol.find_opt db
  |> Lwt_result.map (Option.map ~f:decode)
;;

let read ~id db =
  Query.select ~from:table fields
  |> Query.where Expr.(f_id = i id)
  |> Request.make_zero_or_one
  |> Petrol.find_opt db
  |> Lwt_result.map (Option.map ~f:decode)
;;

(* let read ~id = *)
(*   Base. *)

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

let post_router (request : Dream.request) =
  let* form_data = get_form request in
  let* username = find_data form_data "username" in
  let* display_name = find_data form_data "display_name" in
  let* password = find_data form_data "password" in
  Dream.sql request (insert ~username ~display_name ~password)
;;

let make_input ~name ~text ~input_type =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "mb-6" ] ]
    [ label
        ~a:
          [ a_label_for name
          ; a_class [ "block mb-2 text-sm font-medium text-gray-900 dark:text-white" ]
          ]
        [ txt text ]
    ; input
        ~a:
          [ a_id name
          ; a_name name
          ; a_input_type input_type
          ; a_class
              [ "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg \
                 focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 \
                 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 \
                 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
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
        ; make_input ~name:"display_name" ~text:"Display Name:" ~input_type:`Text
        ; make_input ~name:"password" ~text:"Password:" ~input_type:`Password
        ; button
            ~a:[ a_button_type `Submit; Unsafe.string_attrib "disabled" "" ]
            [ txt "Submit" ]
        ]
    ]
;;
