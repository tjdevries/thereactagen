open Base
open Tyxml.Html

module Head = struct
  type attributes = Html_types.thead_attrib Tyxml_html.attrib list
  type children = Html_types.thead_content_fun elt list_wrap

  let make ?(a = []) children = thead ~a children
end

module Body = struct
  type attributes = Html_types.tbody_attrib Tyxml_html.attrib list
  type children = Html_types.tbody_content_fun elt list_wrap

  let base_classes = [ "bg-slate-50"; "dark:bg-slate-800" ]

  let make ?(a = []) children =
    let attributes = Util.merge_attribute_list [ "class", base_classes ] a in
    tbody ~a:attributes children
  ;;
end

module Row = struct
  type attributes = Html_types.tr_attrib Tyxml_html.attrib list
  type children = Html_types.tr_content_fun elt list_wrap

  let hover_classes = [ "dark:hover:bg-slate-700"; "hover:bg-slate-200/40" ]

  type hover_style =
    | Highlight
    | None

  let classes_of_hover_style = function
    | Highlight -> hover_classes
    | None -> []
  ;;

  let make ?(a = []) ?(hover_style = Highlight) children =
    let attributes =
      Util.merge_attribute_list
        [ "class", classes_of_hover_style hover_style ]
        a
    in
    tr ~a:attributes children
  ;;
end

module Header_cell = struct
  type attributes = Html_types.th_attrib Tyxml_html.attrib list
  type children = Html_types.th_content_fun elt list_wrap

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

  let make ?(a = []) children =
    let attributes = a @ [ a_class base_classes ] in
    th ~a:attributes children
  ;;
end

module Data_cell = struct
  type attributes = Html_types.td_attrib Tyxml_html.attrib list
  type children = Html_types.td_content_fun elt list_wrap

  let base_classes =
    [ "border-b"
    ; "border-slate-100"
    ; "p-4"
    ; "first:pl-8"
    ; "last:pr-8"
    ; "dark:border-slate-700"
    ]
  ;;

  let make ?(a = []) children =
    let attributes = Util.merge_attribute_list [ "class", base_classes ] a in
    td ~a:attributes children
  ;;
end

type attributes = Html_types.tablex_attrib Tyxml_html.attrib list
type children = Html_types.tablex_content_fun elt list_wrap

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

(** Constructs a table using the provided [children].
    - [thead] and [tfoot] are optional table headers and footers, respectively.
    - The table appearance is determined by the [variant]:
      * [Unstyled]: The table is rendered without any specific styling.
      * [Fixed] or [Responsive]: The table adapts to these styles, but [tfoot] is ignored in these cases.

    Additional custom styling can be provided via [classes] and HTML attributes via [attributes]. *)
let make ?(a = []) ?(variant = Responsive) ?thead ?tfoot children =
  if variant_is_unstyled variant
  then tablex ~a ?thead ?tfoot children
  else (
    let merged_classes =
      [ base_classes; classes_of_variant variant ] |> List.concat
    in
    let attributes = Util.merge_attribute_list [ "class", merged_classes ] a in
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
      [ tablex ~a:attributes ?thead children ])
;;
