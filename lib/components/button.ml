open Tyxml.Html

type children = Html_types.button_content_fun elt list_wrap
type attributes = Html_types.button_attrib Tyxml_html.attrib list

type size =
  | Small
  | Medium
  | Large

type variant =
  | Destructive
  | Destructive_outline
  | Ghost
  | Primary
  | Primary_outline
  | Secondary
  | Secondary_outline
  | Twitch

let base_classes =
  [ "flex"
  ; "flex-row"
  ; "items-center"
  ; "gap-2"
  ; "rounded"
  ; "shadow"
  ; "active:scale-95"
  ]
;;

let classes_of_variant = function
  | Primary ->
    [ "bg-orange-400"
    ; "text-white"
    ; "hover:bg-orange-500"
    ; "active:bg-orange-300"
    ]
  | Primary_outline ->
    [ "border"
    ; "border-orange-400"
    ; "bg-transparent"
    ; "text-orange-400"
    ; "hover:bg-orange-200"
    ; "hover:text-orange-500"
    ; "active:bg-orange-100"
    ]
  | Secondary ->
    [ "bg-slate-400"
    ; "text-white"
    ; "hover:bg-slate-500"
    ; "active:bg-slate-300"
    ]
  | Secondary_outline ->
    [ "border-slate-400"
    ; "bg-transparent"
    ; "text-slate-400"
    ; "hover:bg-slate-200"
    ; "hover:text-slate-500"
    ; "active:bg-slate-100"
    ]
  | Destructive ->
    [ "bg-red-500"; "text-white"; "hover:bg-red-600"; "active:bg-red-400" ]
  | Destructive_outline ->
    [ "border-red-500"
    ; "bg-transparent"
    ; "text-red-500"
    ; "hover:bg-red-200"
    ; "hover:text-red-600"
    ; "active:bg-red-100"
    ]
  | Twitch ->
    [ "bg-[#772ce8]"
    ; "text-white"
    ; "hover:bg-[#5c16c5]"
    ; "active:bg-[#9147ff]"
    ]
  | Ghost ->
    [ "bg-transparent"
    ; "text-slate-500"
    ; "dark:text-slate-400"
    ; "hover:bg-slate-200"
    ; "dark:hover:bg-slate-700"
    ; "active:bg-slate-300"
    ; "dark:active:bg-slate-600"
    ]
;;

let classes_of_size = function
  | Small -> [ "px-2"; "py-0"; "text-sm" ]
  | Medium -> [ "px-3"; "py-1"; "text-base" ]
  | Large -> [ "px-4"; "py-2"; "text-lg" ]
;;

let classes_of_props ~variant ~size =
  List.flatten
    [ base_classes; classes_of_variant variant; classes_of_size size ]
;;

let make ?(a = []) ?(variant = Primary) ?(size = Medium) children =
  let attributes =
    Util.merge_attribute_list [ "class", classes_of_props ~variant ~size ] a
  in
  button ~a:attributes children
;;
