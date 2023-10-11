module Typography = struct
  open Tyxml.Html
  open Css

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

  (* TODO: figure out actual styles *)
  type font_style =
    [ `Sans
    | `Serif
    | `Mono
    ]

  (* TODO: figure out actual base classes *)
  let base_classes = [ "text-slate-900"; "leading-normal" ]

  (* TODO: figoure out actual fonts *)
  let classes_of_font_style = function
    | `Sans -> [ "font-sans" ]
    | `Serif -> [ "font-serif" ]
    | `Mono -> [ "font-mono" ]
  ;;

  let classes_of_size = function
    | `Small -> [ "text-sm" ]
    | `Medium -> [ "text-base" ]
    | `Large -> [ "text-lg" ]
  ;;

  let classes_of_props ~font_style ~size =
    Classes.merge
      [ base_classes; classes_of_font_style font_style; classes_of_size size ]
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
    ?(elt = `P)
    ?(size = `Medium)
    ?(font_style = `Sans)
    ~children
    ()
    =
    let merged_classes =
      Classes.merge [ classes; classes_of_props ~size ~font_style ]
    in
    let elt = elt_of_props elt in
    elt ~a:[ a_class merged_classes ] children
  ;;
end
