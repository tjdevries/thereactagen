open Tyxml.Html

type children = Html_types.button_content_fun elt list_wrap

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
  :  ?classes:string list
  -> ?attributes:Html_types.button_attrib Tyxml_html.attrib list
  -> ?variant:variant
  -> ?size:size
  -> children:children
  -> unit
  -> Html_types.button elt
