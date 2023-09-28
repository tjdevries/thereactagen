(* TODO: Move this to Hx lib? *)
type hx_action =
  { target : Hx.TargetType.t
  ; swap : Hx.SwapType.t
  ; trigger : Hx.TriggerType.t
  }

module Card = struct
  open Tyxml.Html

  type 'children t =
    { children : 'children Tyxml.Html.elt list
    ; cls : string list
    }

  let base_classes =
    [ "rounded-sm"
    ; "border"
    ; "border-solid"
    ; "border-slate-200"
    ; "bg-white"
    ; "shadow-md"
    ]
  ;;

  let classes_of_props props = props.cls @ base_classes
  let make props = div ~a:[ a_class @@ classes_of_props props ] props.children
end

module Button = struct
  open Tyxml.Html

  type 'children t =
    { children : 'children Tyxml.Html.elt list
    ; classes : string list
    ; on_click : hx_action
    ; size : [ `Small | `Medium | `Large ]
    ; variant : [ `Primary | `Secondary | `Destructive ]
    }

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

  let classes_of_props props =
    props.classes
    @ base_classes
    @ classes_of_variant props.variant
    @ classes_of_size props.size
  ;;

  let make props =
    button
      ~a:
        [ a_class @@ classes_of_props props
        ; Hx.target props.on_click.target
        ; Hx.swap props.on_click.swap.attr
        ; Hx.trigger props.on_click.trigger
        ]
      props.children
  ;;
end

module Typography = struct
  open Tyxml.Html

  type 'children t =
    { children : 'children Tyxml.Html.elt list
    ; cls : string list
    ; elt : [ `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `P ] option
    ; size : [ `Small | `Medium | `Large ]
    ; font_style : [ `Sans | `Serif | `Mono ]
    }

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

  let classes_of_props props =
    props.cls
    @ base_classes
    @ classes_of_font_style props.font_style
    @ classes_of_size props.size
  ;;

  let elt_of_props props =
    match props.elt with
    | None | Some `P -> p
    | Some `H1 -> h1
    | Some `H2 -> h2
    | Some `H3 -> h3
    | Some `H4 -> h4
    | Some `H5 -> h5
    | Some `H6 -> h6
  ;;

  let make props =
    let elt = elt_of_props props in
    elt ~a:[ a_class @@ classes_of_props props ] props.children
  ;;
end

module ConfirmDeleteCard = struct
  open Tyxml.Html

  type t =
    { on_confirm : hx_action
    ; on_cancel : hx_action
    }

  let make props =
    let title =
      Typography.make
        { children = [ txt "Are you sure?" ]
        ; cls = []
        ; elt = Some `H1
        ; size = `Medium
        ; font_style = `Sans
        }
    in
    let confirm_button =
      Button.make
        { children = [ txt "Confirm" ]
        ; classes = []
        ; on_click = props.on_confirm
        ; size = `Medium
        ; variant = `Destructive
        }
    in
    let cancel_button =
      Button.make
        { children = [ txt "Cancel" ]
        ; classes = []
        ; on_click = props.on_cancel
        ; size = `Medium
        ; variant = `Secondary
        }
    in
    let button_group =
      div
        ~a:[ a_class [ "flex"; "flex-row"; "gap-2" ] ]
        [ confirm_button; cancel_button ]
    in
    let body =
      div
        ~a:[ a_class [ "flex"; "flex-col"; "gap-2"; "p-4" ] ]
        [ div
            ~a:[ a_class [ "text-slate-900 bg-slate-100" ] ]
            [ title; button_group ]
        ]
    in
    Card.make { children = [ body ]; cls = [] }
  ;;
end
