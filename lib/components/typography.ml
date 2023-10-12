module Typography = struct
  open Tyxml.Html

  type children = Html_types.h6_content_fun Tyxml_html.elt list_wrap

  type elt =
    [ `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    | `P
    ]

  type size =
    [ `Small
    | `Medium
    | `Large
    ]

  type font_style =
    [ `Sans
    | `Serif
    | `Mono
    ]

  type font_weight =
    [ `Thin
    | `ExtraLight
    | `Light
    | `Normal
    | `Medium
    | `SemiBold
    | `Bold
    | `ExtraBold
    | `Black
    ]

  let base_classes = [ "text-slate-600"; "dark:text-slate-400" ]

  let classes_of_font_style = function
    | `Sans -> [ "font-sans" ]
    | `Serif -> [ "font-serif" ]
    | `Mono -> [ "font-mono" ]
    | #font_style -> .
  ;;

  let classes_of_font_weight = function
    | `Thin -> [ "font-thin" ]
    | `ExtraLight -> [ "font-extralight" ]
    | `Light -> [ "font-light" ]
    | `Normal -> [ "font-normal" ]
    | `Medium -> [ "font-medium" ]
    | `SemiBold -> [ "font-semibold" ]
    | `Bold -> [ "font-bold" ]
    | `ExtraBold -> [ "font-extrabold" ]
    | `Black -> [ "font-black" ]
    | #font_weight -> .
  ;;

  let classes_of_size = function
    | `Small -> [ "text-sm" ]
    | `Medium -> [ "text-base" ]
    | `Large -> [ "text-lg" ]
    | #size -> .
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
    | `P -> p
    | `H1 -> h1
    | `H2 -> h2
    | `H3 -> h3
    | `H4 -> h4
    | `H5 -> h5
    | `H6 -> h6
  ;;

  let make
    ?(classes = [])
    ?(as_elt = `P)
    ?(size = `Medium)
    ?(font_style = `Sans)
    ?(font_weight = `Normal)
    ~children
    ()
    =
    let classes' = classes @ classes_of_props ~size ~font_style ~font_weight in
    let elt = elt_of_props as_elt in
    elt ~a:[ a_class classes' ] children
  ;;
end
