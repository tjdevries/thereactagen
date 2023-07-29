let get_form request =
  match%lwt Dream.form request with
  | `Ok form_data -> Lwt.return_ok form_data
  | _ -> Lwt.return_error `Missing_form
;;

open Base
open Lwt_result.Syntax

let schema = Schema.schema

open Petrol
open Petrol.Sqlite3
module Category = Types.Category
module Status = Types.Status

let base_url = "/suggestion"

(** A suggestion *)
type t =
  { id : int [@primary]
  ; user_id : int [@foreign User.table [ User.f_id ]]
  ; title : string
  ; url : string
  ; description : string
  ; category : Category.t
  ; status : Status.t
  }
[@@deriving combust ~name:"suggestions"]

let make_filters xs =
  List.map xs ~f:(fun item -> Expr.(f_status = vl ~ty:Status.petrol_type item))
  |> List.reduce ~f:Expr.( || )
  |> Option.value ~default:Expr.(false_)
;;

let resolved_expr =
  List.map Types.Status.resolved ~f:(fun item ->
    Expr.(f_status = vl ~ty:Status.petrol_type item))
  |> List.reduce ~f:Expr.( || )
  |> Option.value ~default:Expr.(false_)
;;

let unresolved_expr = Expr.(not resolved_expr)

let list_with ~filter db =
  Query.select ~from:table fields
  |> Query.where filter
  |> Request.make_many
  |> Petrol.collect_list db
  |> Lwt_result.map (List.map ~f:decode)
;;

let list_unresolved db = list_with ~filter:unresolved_expr db
let list_resolved db = list_with ~filter:resolved_expr db

let suggestion_row suggestion =
  let id = suggestion.id |> Int.to_string in
  let open Tyxml.Html in
  tr
    [ td [ a ~a:[ a_href @@ "/suggestion/view/" ^ id ] [ txt id ] ]
    ; td [ txt suggestion.url ]
    ; td [ txt suggestion.title ]
    ; td [ txt suggestion.description ]
    ; td [ txt (suggestion.category |> Category.show) ]
    ; td [ txt (suggestion.status |> Status.show) ]
    ]
;;

let spinner () =
  let open Tyxml.Html in
  img
    ~a:[ a_class [ "htmx-indicator" ]; a_id "indicator" ]
    ~src:
      "https://raw.githubusercontent.com/SamHerbert/SVG-Loaders/master/svg-loaders/audio.svg"
    ~alt:"loading"
    ()
;;

let filter_form request =
  let form_id = "suggestion-filter" in
  let open Tyxml.Html in
  let btn =
    div
      ~a:[ Hx.include_ ("#" ^ form_id); Hx.target (Css "#suggestion-table") ]
      [ button
          ~a:
            [ a_class [ "btn"; "button" ]
            ; Hx.post "suggestion/table/view"
            ; Hx.indicator "#indicator"
            ]
          [ txt "Submit" ]
      ]
  in
  let status_buttons =
    List.map Status.all ~f:(fun status ->
      tr
        [ td
            [ input
                ~a:
                  [ a_input_type `Checkbox
                  ; a_name "boxes"
                  ; a_value (status |> Status.encode |> Result.ok_or_failwith)
                  ]
                ()
            ]
        ; td [ txt (Status.show status) ]
        ])
  in
  let frm =
    div
      [ spinner ()
      ; form
          ~a:[ a_id form_id ]
          [ Dream.csrf_tag request |> Unsafe.data
          ; table
              ~thead:(thead [ tr [ th [ txt "" ]; th [ txt "Value" ] ] ])
              status_buttons
          ]
      ]
  in
  div [ frm; btn ]
;;

let show_all request =
  let* filter =
    match%lwt get_form request with
    | Ok form_data ->
      (* Unix.sleep 1; *)
      let filters =
        List.filter_map form_data ~f:(fun (key, value) ->
          if String.(key = "boxes")
          then (
            let status = Status.decode value in
            status |> Result.ok)
          else None)
      in
      make_filters filters |> Lwt.return_ok
    | _ -> unresolved_expr |> Lwt.return_ok
  in
  let open Lwt_result.Syntax in
  let* suggestions = Dream.sql request @@ list_with ~filter in
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
    ~thead:
      (thead
         [ tr
             [ hd "ID"
             ; hd "URL"
             ; hd "Title"
             ; hd "Description"
             ; hd "Category"
             ; hd "Status"
             ]
         ])
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
  Dream.sql
    request
    (create ~user_id ~title ~url ~description ~category ~status:Submitted)
;;

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
            [ Category.make_select ~name:"category" () ]
        ; button
            ~a:[ a_button_type `Submit; a_class [ "btn" ] ]
            [ txt "Submit This" ]
        ]
    ]
;;
