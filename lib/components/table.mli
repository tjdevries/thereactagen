open Base
open Tyxml.Html

module Head : sig
  type attributes = Html_types.thead_attrib Tyxml_html.attrib list
  type children = Html_types.thead_content_fun elt list_wrap

  val make : ?a:attributes -> children -> Html_types.thead elt
end

module Body : sig
  type attributes = Html_types.tbody_attrib Tyxml_html.attrib list
  type children = Html_types.tbody_content_fun elt list_wrap

  val make : ?a:attributes -> children -> Html_types.tbody elt
end

module Row : sig
  type attributes = Html_types.tr_attrib Tyxml_html.attrib list
  type children = Html_types.tr_content_fun elt list_wrap

  type hover_style =
    | Highlight
    | None

  (** Constructs a table row using the provided [children].
      - [classes] is an optional list of CSS classes to style the row.
      - [attributes] is an optional list of button-specific attributes for elements within the row.
      - The row's hover behavior is determined by [hover_style]:
        * [Highlight]: (default) The row gets highlighted on hover.
        * [None]: No hover effect.

      Additional custom styling can be provided via [classes] and specific HTML attributes via [attributes]. *)
  val make
    :  ?a:attributes
    -> ?hover_style:hover_style
    -> children
    -> Html_types.tr elt
end

module Header_cell : sig
  type attributes = Html_types.thead_attrib Tyxml_html.attrib list
  type children = Html_types.th_content_fun elt list_wrap

  val make
    :  ?a:attributes
    -> Html_types.th_content_fun elt list_wrap
    -> Html_types.th elt
end

module Data_cell : sig
  type attributes = Html_types.td_attrib Tyxml_html.attrib list
  type children = Html_types.td_content_fun elt list_wrap

  val make : ?a:attributes -> children -> Html_types.td elt
end

type attributes = Html_types.tablex_attrib Tyxml_html.attrib list
type children = Html_types.tablex_content_fun elt list_wrap

type variant =
  | Fixed
  | Responsive
  | Unstyled

(** Constructs a table using the provided [children].
    - [thead] and [tfoot] are optional table headers and footers, respectively.
    - The table appearance is determined by the [variant]:
      * [Unstyled]: The table is rendered without any specific styling.
      * [Fixed] or [Responsive]: The table adapts to these styles, but [tfoot] is ignored in these cases.

    Additional custom styling can be provided via [classes] and HTML attributes via [attributes]. *)
val make
  :  ?a:attributes
  -> ?variant:variant
  -> ?thead:Html_types.thead elt
  -> ?tfoot:Html_types.tfoot elt
  -> children
  -> [> `Div | `Table ] elt
