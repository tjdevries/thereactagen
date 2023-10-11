(* TODO: Move this to Hx lib? *)
type hx_action =
  { target : Hx.TargetType.t
  ; swap : Hx.SwapType.t
  ; trigger : Hx.TriggerType.t
  }

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
    let title =
      Typography.make
        ~elt:`H1
        ~size:`Large
        ~children:[ txt "Are you sure you want to delete this?" ]
        ()
    in
    let button_group =
      div
        ~a:[ a_class [ "flex"; "flex-row"; "gap-2" ] ]
        [ Button.make ?on_click:on_confirm ~children:[ txt "Delete" ] ()
        ; Button.make ?on_click:on_cancel ~children:[ txt "Cancel" ] ()
        ]
    in
    Card.make
      ~classes:merged_classes
      ~children:
        [ div
            ~a:[ a_class [ "flex"; "flex-col"; "gap-2"; "p-4" ] ]
            [ div
                ~a:
                  [ a_class
                      [ "flex flex-row justify-between text-slate-900 \
                         bg-slate-100"
                      ]
                  ]
                [ title; button_group ]
            ]
        ]
      ()
  ;;
end

module Page = struct
  type on_confirm = hx_action
  type on_cancel = hx_action

  let make ?(classes = []) ?on_confirm ?on_cancel () =
    ConfirmDeleteCard.make ~classes ?on_confirm ?on_cancel ()
  ;;
end
