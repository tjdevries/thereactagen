module Button = struct
  open Tyxml.Html
  open Css

  type size =
    [ `Small
    | `Medium
    | `Large
    ]

  type variant =
    [ `Primary
    | `Secondary
    | `Destructive
    ]

  let base_classes =
    [ "flex"
    ; "flex-row"
    ; "items-center"
    ; "gap-2"
    ; "rounded-md"
    ; "border"
    ; "shadow-sm"
    ]
  ;;

  (* TODO: Figure out actual theme/colors *)
  let classes_of_variant = function
    | `Primary ->
      [ "bg-emerald-500"
      ; "text-white"
      ; "border-emerald-600"
      ; "focus:ring-2"
      ; "hover:bg-emerald-400"
      ; "active:bg-emerald-600"
      ; "active:border-emerald-700"
      ]
    | `Secondary ->
      [ "bg-white"
      ; "text-slate-900"
      ; "border-slate-200"
      ; "hover:bg-slate-50"
      ; "active:bg-slate-100"
      ]
    | `Destructive ->
      [ "bg-red-500"
      ; "text-white"
      ; "border-red-600"
      ; "hover:bg-red-400"
      ; "active:bg-red-600"
      ; "focus:ring-2"
      ]
  ;;

  let classes_of_size = function
    | `Small -> [ "px-2"; "py-1"; "text-sm" ]
    | `Medium -> [ "px-3"; "py-2"; "text-base" ]
    | `Large -> [ "px-4"; "py-3"; "text-lg" ]
  ;;

  let classes_of_props ~variant ~size =
    Classes.merge
      [ base_classes; classes_of_variant variant; classes_of_size size ]
  ;;

  let make
    ?(classes = [])
    ?(variant = `Primary)
    ?(size = `Medium)
    ~hx_route
    ~hx_target
    ~children
    ()
    =
    let hx_classes = [] in
    let merged_classes =
      Classes.merge [ classes; classes_of_props ~variant ~size ]
    in
    button ~a:[ a_class merged_classes ] children
  ;;
end
