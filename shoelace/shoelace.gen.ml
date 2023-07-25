open Base

let x = 10
let carousel children = Tyxml.Html.Unsafe.node "sl-carousel" children
(* loop navigation pagination autoplay autoplay-interval slides-per-page slides-per-move orientation mouse-dragging *)

let carousel ?(loop = false) =
  let loop =
    if loop then Some (Tyxml_html.Unsafe.string_attrib "loop" "") else None
  in
  let attributes = [ loop ] |> List.filter ~f:Option.is_some in
  assert false
;;
