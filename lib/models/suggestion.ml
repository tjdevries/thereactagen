open Base
open Lwt_result.Syntax

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3

module CategoryImpl = struct
  type t =
    | Video
    | Article
    | Website
    | Twitch
  [@@deriving enumerate, show { with_path = false }]

  type storage = string

  let caqti_storage = Caqti_type.string
  let repr = "category"

  let encode_exn = function
    | Article -> "article"
    | Video -> "video"
    | Website -> "website"
    | Twitch -> "twitch"
  ;;

  let decode_exn t =
    match String.lowercase t with
    | "article" -> Article
    | "video" -> Video
    | "website" -> Website
    | "twitch" -> Twitch
    | _ -> raise Stdlib.Not_found
  ;;

  let make_select ~name () =
    let open Tyxml.Html in
    let options =
      List.map all ~f:(fun t -> option ~a:[ a_value (show t) ] (txt @@ show t))
    in
    select
      ~a:[ a_class [ "select"; "select-bordered" ]; a_id name; a_name name ]
      options
  ;;
end

module Category = Mulroy.Make (CategoryImpl)

let base_url = "/suggestion"

(** A suggestion *)
type t =
  { id : int [@primary]
  ; user_id : int [@foreign User.table [ User.f_id ]]
  ; title : string
  ; url : string
  ; description : string
  ; category : Category.t
  }
[@@deriving combust ~name:"suggestions"]

let list_all db =
  Query.select ~from:table fields
  |> Request.make_many
  |> Petrol.collect_list db
  |> Lwt_result.map (List.map ~f:decode)
;;

let suggestion_row suggestion =
  let id = suggestion.id |> Int.to_string in
  let open Tyxml.Html in
  tr
    [ td
        [ a
            ~a:
              [ a_href @@ "/suggestion/view/" ^ id
              ; a_class [ "link link-hover" ]
              ]
            [ txt id ]
        ]
    ; td [ txt suggestion.url ]
    ; td [ txt suggestion.title ]
    ; td [ txt suggestion.description ]
    ]
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
  let hd text = th [ txt text ] in
  table
    ~a:
      [ a_id "suggestion-table"
      ; a_class [ "table table-zebra table-pin-rows" ]
      ]
    ~thead:(thead [ tr [ hd "id"; hd "url"; hd "title"; hd "description" ] ])
    suggestions
  |> Lwt.return_ok
;;

let show_one request id =
  let open Lwt_result.Syntax in
  let conn = Dream.sql request in
  let* suggestion = conn @@ read id in
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
  let* user_id = Reactagen.Auth.get_user request in
  let* url = find_data form_data "url" in
  let* title = find_data form_data "title" in
  let* description = find_data form_data "description" in
  let* category = find_data form_data "category" in
  let category =
    match category |> Category.decode with
    | Ok category -> category
    | _ -> assert false
  in
  Dream.sql request (create ~user_id ~title ~url ~description ~category)
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

let make_input ~name ~text ~input_type =
  let open Tyxml.Html in
  div
    ~a:[ a_class [ "mb-6" ] ]
    [ label
        ~a:[ a_label_for name; a_class [ "label" ] ]
        [ span ~a:[ a_class [ "label-text" ] ] [ txt text ] ]
    ; input
        ~a:
          [ a_id name
          ; a_name name
          ; a_input_type input_type
          ; a_class [ "input input-bordered" ]
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
        [ make_input ~name:"url" ~text:"URL:" ~input_type:`Text
        ; make_input ~name:"title" ~text:"Title:" ~input_type:`Text
        ; make_input ~name:"description" ~text:"Description:" ~input_type:`Text
        ; div
            ~a:[ a_class [ "mb-6" ] ]
            [ CategoryImpl.make_select ~name:"category" () ]
        ; button
            ~a:[ a_button_type `Submit; a_class [ "btn" ] ]
            [ txt "Submit This" ]
        ]
    ]
;;
