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
(* "rounded px-4 py-2 text-slate-500 hover:bg-slate-200 active:scale-95 active:bg-slate-300 dark:text-slate-400 dark:hover:bg-slate-700 dark:active:bg-slate-600" *)
