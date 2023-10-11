module Htmx = struct
  open Tyxml_html

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

  let hx_delete uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-delete"
  let hx_get uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-get"
  let hx_patch uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-patch"
  let hx_post uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-post"
  let hx_put uri = uri |> Uri.to_string |> Unsafe.string_attrib "hx-put"

  let route_to_attr = function
    | `Delete uri -> hx_delete uri
    | `Get uri -> hx_get uri
    | `Patch uri -> hx_patch uri
    | `Post uri -> hx_post uri
    | `Put uri -> hx_put uri
  ;;

  let to_attr = function
    | `Target target -> Hx.target target
    | `Swap swap -> Hx.SwapType.(Hx.swap swap.attr)
    | `Trigger trigger -> Hx.trigger trigger
    | `Route route -> route_to_attr route
  ;;
end
