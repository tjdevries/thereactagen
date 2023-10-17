open Tyxml.Html

type attributes = Html_types.div_attrib Tyxml_html.attrib list
type children = Html_types.div_content_fun elt list_wrap

let base_classes =
  [ "p-6"
  ; "rounded-xl"
  ; "bg-white"
  ; "dark:bg-slate-800"
  ; "shadow-xl"
  ; "border"
  ; "border-slate-200/60"
  ; "dark:border-slate-700/20"
  ]
;;

let make ?(a = []) children =
  let attributes = Util.merge_attribute_list [ "class", base_classes ] a in
  div ~a:attributes children
;;
