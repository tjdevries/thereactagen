(* TODO: Move this to Hx lib? *)
type hx_action =
  { target : Hx.TargetType.t
  ; swap : Hx.SwapType.t
  ; trigger : Hx.TriggerType.t
  }

module Class = struct
  module StringSet = Set.Make (String)

  let merge (classes : string list list) =
    classes
    |> List.flatten
    |> StringSet.of_list
    |> StringSet.to_seq
    |> List.of_seq
  ;;
end

module Attributes = struct
  let of_hx_action (action : hx_action) =
    [ Hx.target action.target
    ; Hx.swap action.swap.attr
    ; Hx.trigger action.trigger
    ]
  ;;
end

module Card = struct
  open Tyxml.Html

  let base_classes =
    [ "rounded-sm"
    ; "border"
    ; "border-solid"
    ; "border-slate-200"
    ; "bg-white"
    ; "shadow-md"
    ]
  ;;

  let make ?(classes = []) ~children () =
    let merged_classes = Class.merge [ base_classes; classes ] in
    div ~a:[ a_class merged_classes ] children
  ;;
end

module Button = struct
  open Tyxml.Html

  type on_click = hx_action

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
    Class.merge
      [ base_classes; classes_of_variant variant; classes_of_size size ]
  ;;

  let make
    ?(classes = [])
    ?(variant = `Primary)
    ?(size = `Medium)
    ?on_click
    ~children
    ()
    =
    let merged_classes =
      Class.merge [ classes; classes_of_props ~variant ~size ]
    in
    let attrs =
      on_click
      |> Option.map Attributes.of_hx_action
      |> Option.value ~default:[]
      |> List.append [ a_class merged_classes ]
    in
    button ~a:attrs children
  ;;
end

module Typography = struct
  open Tyxml.Html

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

  let base_classes = [ "text-slate-900"; "leading-normal" ]

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
    Class.merge
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
      Class.merge [ classes; classes_of_props ~size ~font_style ]
    in
    let elt = elt_of_props elt in
    elt ~a:[ a_class merged_classes ] children
  ;;
end

let _ =
  Typography.make
    ~elt:`H1
    ~size:`Large
    ~children:[ Tyxml.Html.txt "Hello, world!" ]
    ()
;;

module ConfirmDeleteCard = struct
  open Tyxml.Html

  type on_confirm = hx_action
  type on_cancel = hx_action

  let make ?(classes = []) ?on_confirm ?on_cancel () =
    let merged_classes = Class.merge [ [ "w-1/3"; "h-40" ]; classes ] in
    let button_group =
      div
        ~a:[ a_class [ "flex"; "flex-row"; "gap-2" ] ]
        [ Button.make ?on_click:on_confirm ~children:[ txt "Delete" ] ()
        ; Button.make ?on_click:on_cancel ~children:[ txt "Cancel" ] ()
        ]
    in
    let card_body =
      div
        ~a:[ a_class [ "flex"; "flex-col"; "gap-2"; "p-4" ] ]
        [ div
            ~a:
              [ a_class
                  [ "flex flex-row justify-between text-slate-900 bg-slate-100"
                  ]
              ]
            [ button_group ]
        ]
    in
    Card.make ~classes:merged_classes ~children:[ card_body ] ()
  ;;
end

module Page = struct
  type on_confirm = hx_action
  type on_cancel = hx_action

  let make ?(classes = []) ?on_confirm ?on_cancel () =
    ConfirmDeleteCard.make ~classes ?on_confirm ?on_cancel ()
  ;;
end
