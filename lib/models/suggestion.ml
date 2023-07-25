open Base
open Lwt_result.Syntax

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

type category =
  | Video
  | Article

let category_to_string = function
  | Article -> "article"
  | Video -> "video"
;;

let category_of_string = function
  | "article" -> Article
  | "video" -> Video
  | _ -> assert false
;;

let base_url = "/suggestion"

(** A suggestion *)
type t =
  { id : int [@primary]
  ; user_id : int [@foreign Models.User f_id]
  ; url : string
  ; description : string
  ; category : category
  }
[@@deriving model ~name:"suggestions"]

(* We could probably do `let table, fields = ...` and then
   folow that up with `let [ f_id, f_x, ... ] = fields *)
let table, fields =
  StaticSchema.declare_table
    schema
    ~name:"suggestions"
    Schema.
      [ field
          "id"
          ~ty:Type.int
          ~constraints:[ primary_key ~auto_increment:true () ]
      ; field
          "user_id"
          ~ty:Type.int
          ~constraints:
            [ foreign_key
                ~table:User.table
                ~columns:Expr.[ User.f_id ]
                ~on_update:`RESTRICT
                ~on_delete:`RESTRICT
                ()
            ]
      ; field "url" ~ty:Type.text
      ; field "description" ~ty:Type.text
      ; field "category" ~ty:Type.text
      ]
;;

(* let Expr.[ f_id; f_user_id; f_url; f_description; f_category ] = fields *)
let Expr.[ f_id; f_user_id; f_url; f_description; f_category ] = fields

let decode (id, (user_id, (url, (description, (category, ()))))) =
  { id; user_id; url; description; category = category_of_string category }
;;

let insert ~user_id ~url ~description ~category db =
  Query.insert
    ~table
    ~values:
      Expr.
        [ f_user_id := i user_id
        ; f_url := s url
        ; f_description := s description
        ; f_category := s (category_to_string category)
        ]
  |> Query.returning Expr.[ f_id ]
  |> Request.make_one
  |> Petrol.find db
  |> Lwt_result.map fst
;;

let list_all db =
  Query.select ~from:table fields
  |> Request.make_many
  |> Petrol.collect_list db
  |> Lwt_result.map (List.map ~f:decode)
;;

let find ~id db =
  Query.select ~from:table fields
  |> Query.where Expr.(f_id = i id)
  |> Request.make_zero_or_one
  |> Petrol.find_opt db
  |> Lwt_result.map (Option.map ~f:decode)
;;

let suggestion_row suggestion =
  let open Tyxml.Html in
  tr [ td [ txt suggestion.url ]; td [ txt suggestion.description ] ]
;;

let show_all request =
  let open Lwt_result.Syntax in
  let conn = Dream.sql request in
  let* suggestions = conn list_all in
  let open Tyxml.Html in
  let suggestions =
    List.fold_left suggestions ~init:[] ~f:(fun acc suggestion ->
      suggestion_row suggestion :: acc)
    |> List.rev
  in
  table
    ~a:[ a_id "suggestion-table"; a_class [ "table table-xs" ] ]
    ~thead:(thead [ tr [ th [ txt "url" ]; th [ txt "description" ] ] ])
    suggestions
  |> Lwt.return_ok
;;

let show_one request id =
  let open Lwt_result.Syntax in
  let conn = Dream.sql request in
  let* suggestion = conn @@ find ~id in
  let text =
    match suggestion with
    | Some suggestion -> suggestion_row suggestion
    | None -> assert false
  in
  Lwt.return_ok text
;;

let get_form request =
  match%lwt Dream.form request with
  | `Ok form_data -> Lwt.return_ok form_data
  | _ -> Lwt.return_error `Missing_form
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

let post_router (request : Dream.request) =
  let* form_data = get_form request in
  let* user_id = find_data form_data "user_id" in
  let user_id = user_id |> Int.of_string in
  let* url = find_data form_data "url" in
  let* description = find_data form_data "description" in
  let* category = find_data form_data "category" in
  Dream.sql
    request
    (insert ~user_id ~url ~description ~category:(category_of_string category))
;;

module Shoelace = struct
  module Unsafe = Tyxml.Html.Unsafe

  let input ~name ~lbl ~input_type =
    let open Tyxml.Html in
    Unsafe.node
      "sl-input"
      ~a:
        [ a_id name
        ; a_name name
        ; Unsafe.string_attrib "label" lbl
        ; Unsafe.string_attrib "type" input_type
        ]
      []
  ;;

  let button ~a text = Unsafe.node "sl-button" ~a [ Tyxml.Html.txt text ]
end

(* [ (* This will be from the session later *) *)
(*   Shoelace.input ~name:"user_id" ~lbl:"UserID:" ~input_type:"text" *)
(* ; Shoelace.input ~name:"url" ~lbl:"Suggestion URL:" ~input_type:"text" *)
(* ; Shoelace.input *)
(*     ~name:"description" *)
(*     ~lbl:"Brief Description:" *)
(*     ~input_type:"text" *)
(* ; (* TODO: Make a list of choices *) *)
(*   Shoelace.input ~name:"category" ~lbl:"Category:" ~input_type:"text" *)
(* ; Shoelace.button ~a:[ a_button_type `Submit ] "Submit This" *)
(* ] *)

(* <div class="mb-6"> *)
(*     <label for="email" class="block mb-2 text-sm font-medium text-gray-900 dark:text-white">Your email</label> *)
(*     <input type="email" id="email" class="shadow-sm bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500 dark:shadow-sm-light" placeholder="name@flowbite.com" required> *)
(*   </div> *)
(* <div class="mx-auto max-w-xs"> *)
(*   <div> *)
(*     <label for="example1" class="mb-1 block text-sm font-medium text-gray-700">Email</label> *)
(*     <input type="email" id="example1" class="block w-full rounded-md border-gray-300 shadow-sm focus:border-primary-400 focus:ring focus:ring-primary-200 focus:ring-opacity-50 disabled:cursor-not-allowed disabled:bg-gray-50 disabled:text-gray-500" placeholder="you@email.com" /> *)
(*   </div> *)
(* </div> *)

let button_class =
  "rounded-lg border border-primary-500 bg-primary-500 px-5 py-2.5 text-center \
   text-sm font-medium text-black shadow-sm transition-all \
   hover:border-primary-700 hover:bg-primary-700 focus:ring \
   focus:ring-primary-200 disabled:cursor-not-allowed \
   disabled:border-primary-300 disabled:bg-primary-300"
;;

let make_input ~name ~text ~input_type =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "mb-6" ] ]
    [ label
        ~a:
          [ a_label_for name
          ; a_class [ "mb-1 block text-sm font-medium text-gray-700" ]
          ]
        [ txt text ]
    ; input
        ~a:
          [ a_id name
          ; a_name name
          ; a_input_type input_type
          ; a_class
              [ "block w-full rounded-md border-gray-300 shadow-sm \
                 focus:border-primary-400 focus:ring focus:ring-primary-200 \
                 focus:ring-opacity-50 disabled:cursor-not-allowed \
                 disabled:bg-gray-50 disabled:text-gray-500"
              ]
          ]
        ()
    ]
;;

let post_form request =
  let open Tyxml.Html in
  form
    ~a:[ Hx.post base_url; Hx.swap BeforeEnd; Hx.target (Previous "tbody") ]
    [ Dream.csrf_tag request |> Unsafe.data
    ; div
        ~a:[ a_id "suggestion-form" ]
        [ make_input ~name:"user_id" ~text:"User ID:" ~input_type:`Text
        ; make_input ~name:"url" ~text:"URL:" ~input_type:`Text
        ; make_input ~name:"description" ~text:"Description:" ~input_type:`Text
        ; make_input ~name:"category" ~text:"Category:" ~input_type:`Text
        ; button
            ~a:[ a_button_type `Submit; a_class [ button_class ] ]
            [ txt "Submit This" ]
        ]
    ]
;;
