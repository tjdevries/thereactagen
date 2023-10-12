open Tyxml.Html

type children = Html_types.button_content_fun elt list_wrap

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

val make
  :  ?classes:string list
  -> ?variant:variant
  -> ?size:size
  -> hx_route:Attributes.Htmx.route
  -> hx_target:Hx.TargetType.t
  -> children:children
  -> unit
  -> Html_types.button elt
