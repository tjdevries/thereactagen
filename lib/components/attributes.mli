module Htmx : sig
  type route =
    [ `Delete of Uri.t
    | `Get of Uri.t
    | `Patch of Uri.t
    | `Post of Uri.t
    | `Put of Uri.t
    ]

  type t =
    [ `Target of Hx.TargetType.t
    | `Swap of Hx.SwapType.t
    | `Trigger of Hx.TriggerType.t
    | `Route of route
    ]

  val to_attr : t -> 'a Tyxml_html.attrib
end
