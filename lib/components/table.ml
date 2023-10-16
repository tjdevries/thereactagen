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
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    tr ~a:attrs children
  ;;
end

module Header_cell = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    th ~a:attrs children
  ;;
end

module Data_cell = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    td ~a:attrs children
  ;;
end

let wrapper_classes =
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
;;

let table_classes = [ "border-collapse"; "w-full"; "h-full"; "text-sm" ]

module Responsive = struct
  let make children =
    let wrapper = div ~a:[ a_class wrapper_classes ] in
    wrapper
      [ tablex ~a:[ a_class (table_classes @ [ "table-auto" ]) ] children ]
  ;;
end

module Fixed = struct
  let make children =
    let wrapper = div ~a:[ a_class wrapper_classes ] in
    wrapper
      [ tablex ~a:[ a_class (table_classes @ [ "table-fixed" ]) ] children ]
  ;;
end

module Element = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    table ~a:attrs children
  ;;
end

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
