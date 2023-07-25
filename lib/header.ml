let default_header title_txt =
  let open Tyxml.Html in
  head (title @@ txt title_txt) Scripts.[ htmx; tailwind; daisy; mystyle ]
;;
