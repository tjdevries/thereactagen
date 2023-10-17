open Base

let html_to_string html = Fmt.str "%a" (Tyxml.Html.pp ()) html
let elt_to_string elt = Fmt.str "%a" (Tyxml.Html.pp_elt ()) elt

(** Processes and merges multiple attribute name and value pairs into a provided attribute list.
    - [attribute_pairs] consists of a list of tuples, where each tuple represents an attribute name and its corresponding list of values to be merged.
    - [attributes] is the list of existing attributes that will be updated or appended with new ones from [attribute_pairs].

    Each tuple in [attribute_pairs] will undergo the same merge or insertion process as defined in [merge_attribute_values]. *)
let merge_attribute_list attribute_pairs attributes =
  attribute_pairs
  |> List.fold
       ~init:(Tyxml.Html.to_xmlattribs attributes)
       ~f:(fun acc (name, values) ->
         Tyxml.Xml.add_string_attrib name (String.concat ~sep:" " values) acc)
  |> List.map ~f:Tyxml.Html.to_attrib
;;
