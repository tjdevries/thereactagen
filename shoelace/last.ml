module Unsafe = Tyxml.Html.Unsafe

let x = {|<sl-button size="small">Click me</sl-button>|}
let button ~a children = Unsafe.node "sl-button" ~a children
let input () = Unsafe.node "sl-input" []
