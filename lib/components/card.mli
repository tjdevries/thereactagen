open Tyxml.Html

type attributes = Html_types.div_attrib Tyxml_html.attrib list
type children = Html_types.div_content_fun elt list_wrap

val make : ?a:attributes -> children -> [> Html_types.div ] elt
