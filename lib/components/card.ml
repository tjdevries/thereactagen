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

let make ?(classes = []) ?(attributes = []) children =
  let classes' = base_classes @ classes in
  let attrs = attributes @ [ a_class classes' ] in
  div ~a:attrs children
;;
