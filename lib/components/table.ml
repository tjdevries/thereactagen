[@@@ocaml.warning "-26-27-34"]

open Base
open Tyxml.Html

module Head = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    thead ~a:attrs children
  ;;
end

module Body = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    tbody ~a:attrs children
  ;;
end

module Row = struct
  let hover_classes = [ "dark:hover:bg-slate-700"; "hover:bg-slate-50" ]

  type hover_style =
    | Highlight
    | None

  let classes_of_hover_style = function
    | Highlight -> hover_classes
    | None -> []
  ;;

  let make
    ?(classes = [])
    ?(attributes = [])
    ?(hover_style = Highlight)
    children
    =
    let hover_classes' = classes_of_hover_style hover_style in
    let attrs = attributes @ [ a_class (classes @ hover_classes') ] in
    tr ~a:attrs children
  ;;
end

module Header_cell = struct
  let base_classes =
    [ "border-b"
    ; "p-4"
    ; "pb-3"
    ; "first:pl-8"
    ; "last:pr-8"
    ; "pt-0"
    ; "text-left"
    ; "font-medium"
    ; "dark:border-slate-600"
    ]
  ;;

  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class (classes @ base_classes) ] in
    th ~a:attrs children
  ;;
end

module Data_cell = struct
  let base_classes =
    [ "border-b"
    ; "border-slate-100"
    ; "p-4"
    ; "first:pl-8"
    ; "last:pr-8"
    ; "dark:border-slate-700"
    ]
  ;;

  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class (classes @ base_classes) ] in
    td ~a:attrs children
  ;;
end

type variant =
  | Fixed
  | Responsive
  | Unstyled

let classes_of_variant = function
  | Unstyled -> []
  | Fixed -> [ "table-fixed" ]
  | Responsive -> [ "table-auto" ]
;;

let variant_is_unstyled = function
  | Unstyled -> true
  | _ -> false
;;

let base_classes = [ "border-collapse"; "w-full"; "h-full"; "text-sm" ]

let table_wrapper =
  div
    ~a:
      [ a_class
          [ "h-full"
          ; "w-full"
          ; "overflow-auto"
          ; "rounded-xl"
          ; "border"
          ; "border-black/5"
          ; "bg-slate-100"
          ; "py-8"
          ; "text-slate-500"
          ; "shadow-sm"
          ; "dark:border-white/5"
          ; "dark:bg-slate-900/40"
          ; "dark:text-slate-400"
          ]
      ]
;;

let make ?(classes = []) ?(attributes = []) ?(variant = Responsive) children =
  if variant_is_unstyled variant
  then tablex ~a:(attributes @ [ a_class classes ]) children
  else
    table_wrapper
      [ tablex
          ~a:
            (attributes
             @ [ a_class (classes @ base_classes @ classes_of_variant variant) ]
            )
          children
      ]
;;

(* let make
   (type data)
   ~(columns : (data, 'a) Column_spec.t list)
   ~(data : data list)
   ()
   =
   let thead =
   columns
   |> List.map ~f:Column_spec.to_header_cell
   |> Row.make
   |> List.return
   |> Head.make
   in
   let tbody =
   data
   |> List.map ~f:(fun datum ->
   columns |> List.map ~f:(Column_spec.to_data_cell datum))
   |> List.map ~f:Row.make
   |> Body.make
   |> List.return
   in
   tablex ~a:[ a_class [ table_classes ] ] ~thead tbody
   ;; *)

(* module Column_spec = struct
  type ('data, 'column) t =
    { header : string
    ; accessor : 'data -> 'column
    ; to_string : 'column -> string
    }

  let header { header; _ } = header
  let accessor { accessor; _ } = accessor
  let value { accessor; _ } data = accessor data

  let to_header_cell column =
    column.header |> txt |> List.return |> Header_cell.make
  ;;

  let to_data_cell datum column =
    datum
    |> column.accessor
    |> column.to_string
    |> txt
    |> List.return
    |> Data_cell.make
  ;;
end *)
