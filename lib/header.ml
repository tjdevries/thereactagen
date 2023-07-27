let default_header title_txt =
  let open Tyxml.Html in
  head (title @@ txt title_txt) Scripts.[ htmx; tailwind; daisy; mystyle ]
;;

let html title_ inner =
  let open Tyxml.Html in
  html
    ~a:[ Unsafe.string_attrib "data-theme" "night" ]
    (default_header title_)
    (body inner)
;;
