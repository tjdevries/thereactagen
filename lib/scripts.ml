open Tyxml.Html

let shoelace =
  script
    ~a:
      [ Unsafe.string_attrib "type" "module"
      ; a_src
          "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.5.2/cdn/shoelace-autoloader.js"
      ]
    (txt "")
;;

let shoelace_style =
  link
    ~rel:[ `Stylesheet ]
    ~href:
      "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.5.2/cdn/themes/light.css"
    ()
;;

let tailwind = script ~a:[ a_src "https://cdn.tailwindcss.com" ] (txt "")
let htmx = script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.0" ] (txt "")

let daisy =
  link
    ~rel:[ `Stylesheet ]
    ~href:"https://cdn.jsdelivr.net/npm/daisyui@3.3.1/dist/full.css"
    ()
;;

let mystyle = style [ txt {| :not(:defined) {
  visibility: hidden;
} |} ]
