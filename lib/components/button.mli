open Tyxml.Html

type children = Html_types.button_content_fun elt list_wrap
type attributes = Html_types.button_attrib Tyxml_html.attrib list

type size =
  | Small
  | Medium
  | Large

type variant =
  | Destructive
  | Destructive_outline
  | Ghost
  | Primary
  | Primary_outline
  | Secondary
  | Secondary_outline
  | Twitch

val make
  :  ?a:attributes
  -> ?variant:variant
  -> ?size:size
  -> children
  -> Html_types.button elt
