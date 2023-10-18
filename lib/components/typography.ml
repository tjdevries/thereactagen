open Tyxml.Html

type children = Html_types.phrasing Tyxml_html.elt list_wrap
type attributes = Html_types.common Tyxml_html.attrib list

type elt =
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | P

type size =
  | Small
  | Medium
  | Large

type font_style =
  | Sans
  | Serif
  | Mono

type font_weight =
  | Thin
  | Extra_light
  | Light
  | Normal
  | Medium
  | Semi_bold
  | Bold
  | Extra_bold
  | Black

let base_classes = [ "text-slate-600"; "dark:text-slate-400" ]

let classes_of_font_style = function
  | Sans -> [ "font-sans" ]
  | Serif -> [ "font-serif" ]
  | Mono -> [ "font-mono" ]
;;

let classes_of_font_weight = function
  | Thin -> [ "font-thin" ]
  | Extra_light -> [ "font-extralight" ]
  | Light -> [ "font-light" ]
  | Normal -> [ "font-normal" ]
  | Medium -> [ "font-medium" ]
  | Semi_bold -> [ "font-semibold" ]
  | Bold -> [ "font-bold" ]
  | Extra_bold -> [ "font-extrabold" ]
  | Black -> [ "font-black" ]
;;

let classes_of_size = function
  | Small -> [ "text-xs" ]
  | Medium -> [ "text-base" ]
  | Large -> [ "text-2xl" ]
;;

let classes_of_props ~font_style ~font_weight ~size =
  List.flatten
    [ base_classes
    ; classes_of_font_style font_style
    ; classes_of_size size
    ; classes_of_font_weight font_weight
    ]
;;

let elt_of_props elt =
  match elt with
  | P -> p
  | H1 -> h1
  | H2 -> h2
  | H3 -> h3
  | H4 -> h4
  | H5 -> h5
  | H6 -> h6
;;

let make
  ?(a = [])
  ?(as_elt = P)
  ?(size : size = Medium)
  ?(font_style = Sans)
  ?(font_weight = Normal)
  children
  =
  let classes = classes_of_props ~size ~font_style ~font_weight in
  let attributes = Util.merge_attribute_list [ "class", classes ] a in
  let elt = elt_of_props as_elt in
  elt ~a:attributes children
;;
