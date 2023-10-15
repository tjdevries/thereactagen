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

module Column_spec = struct
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
end

module Responsive_table = struct
  let make
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
    tablex ~thead tbody
  ;;
end

module Fixed = struct
  let make
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
    tablex ~thead tbody
  ;;
end

module Element = struct
  let make ?(classes = []) ?(attributes = []) children =
    let attrs = attributes @ [ a_class classes ] in
    table ~a:attrs children
  ;;
end
