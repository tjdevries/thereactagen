[@@@ocaml.warning "-37"]

open Tyxml.Html
open Components
(* open! Reload_tailwind *)

module Toggle_theme_button = struct
  type theme =
    | Light
    | Dark

  let theme_to_string = function
    | Light -> "light"
    | Dark -> "dark"
  ;;

  let theme_of_string = function
    | "light" -> Light
    | "dark" -> Dark
    | _ -> Dark
  ;;

  let next_theme = function
    | Light -> Dark
    | Dark -> Light
  ;;

  let endpoint = Uri.of_string "/toggle-theme"

  let make next_theme =
    let theme = theme_to_string next_theme in
    Button.make
      ~a:
        [ Hx.V2.Attributes.target (Css_selector "#theme")
        ; Hx.V2.Attributes.swap { strategy = OuterHTML; modifier = None }
        ; Hx.V2.Attributes.post (Uri.with_query' endpoint [ "theme", theme ])
        ]
      ~variant:Primary
      [ txt "Toggle theme" ]
  ;;
end

(* TODO: Split this out into the component-library lib w/ better helpers + abstractions *)
let index ?(theme = Toggle_theme_button.Dark) () =
  html
    (head
       (title (txt "Component Library Playground"))
       [ script ~a:[ a_src "https://unpkg.com/htmx.org@1.9.0" ] (txt "")
       ; link ~rel:[ `Stylesheet ] ~href:"static/output.css" ()
       ])
    (body
       [ div
           ~a:
             [ a_id "theme"
             ; a_class [ Toggle_theme_button.theme_to_string theme ]
             ]
           [ div
               ~a:
                 [ a_class
                     [ "w-full"
                     ; "h-full"
                     ; "flex"
                     ; "flex-col"
                     ; "dark:bg-slate-900"
                     ; "dark:text-slate-400"
                     ; "spacey-4"
                     ]
                 ]
               [ div
                   ~a:
                     [ a_class
                         [ "flex flex-row justify-end items-end pt-4 px-4" ]
                     ]
                   [ Toggle_theme_button.make
                       (Toggle_theme_button.next_theme theme)
                   ]
               ; div
                   ~a:
                     [ a_class
                         [ "w-full"
                         ; "grid"
                         ; "grid-cols-2"
                         ; "justify-items-center"
                         ; "justify-items-center"
                         ; "gap-4"
                         ; "p-8"
                         ]
                     ]
                   [ Card.make
                       ~a:[ a_class [ "w-full" ] ]
                       [ Typography.make
                           ~a:
                             [ a_class
                                 [ "mb-1"
                                 ; "border-b"
                                 ; "border-b-slate-400"
                                 ; "pb-2"
                                 ]
                             ]
                           ~as_elt:H1
                           ~size:Large
                           ~font_weight:Bold
                           [ txt "Typography" ]
                       ; div
                           ~a:
                             [ a_class [ "flex flex-row justify-between p-4" ] ]
                           [ div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Font styles" ]
                               ; ul
                                   ~a:[ a_class [ "list-disc"; "ml-2" ] ]
                                   [ li
                                       [ Typography.make
                                           ~font_style:Mono
                                           [ txt "Mono" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_style:Serif
                                           [ txt "Serif" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_style:Sans
                                           [ txt "Sans" ]
                                       ]
                                   ]
                               ]
                           ; div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Font sizes" ]
                               ; ul
                                   ~a:[ a_class [ "list-disc"; "ml-2" ] ]
                                   [ li
                                       [ Typography.make
                                           ~size:Small
                                           [ txt "Small" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~size:Medium
                                           [ txt "Medium" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~size:Large
                                           [ txt "Large" ]
                                       ]
                                   ]
                               ]
                           ]
                       ; div
                           ~a:
                             [ a_class [ "flex flex-row justify-between p-4" ] ]
                           [ div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Font weights" ]
                               ; ul
                                   ~a:[ a_class [ "list-disc"; "ml-2" ] ]
                                   [ li
                                       [ Typography.make
                                           ~font_weight:Thin
                                           [ txt "Thin" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Extra_light
                                           [ txt "ExtraLight" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Light
                                           [ txt "Light" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Normal
                                           [ txt "Normal" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Medium
                                           [ txt "Medium" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Semi_bold
                                           [ txt "SemiBold" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Bold
                                           [ txt "Bold" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Extra_bold
                                           [ txt "ExtraBold" ]
                                       ]
                                   ; li
                                       [ Typography.make
                                           ~font_weight:Black
                                           [ txt "Black" ]
                                       ]
                                   ]
                               ]
                           ]
                       ]
                   ; Card.make
                       ~a:[ a_class [ "w-full" ] ]
                       [ Typography.make
                           ~a:
                             [ a_class
                                 [ "mb-1"
                                 ; "border-b"
                                 ; "border-b-slate-400"
                                 ; "pb-2"
                                 ]
                             ]
                           ~as_elt:H1
                           ~size:Large
                           ~font_weight:Bold
                           [ txt "Buttons" ]
                       ; div
                           ~a:
                             [ a_class [ "flex flex-row justify-between p-4" ] ]
                           [ div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Variants" ]
                               ; ul
                                   ~a:
                                     [ a_class
                                         [ "list-disc"; "ml-2"; "space-y-2" ]
                                     ]
                                   [ li
                                       [ Button.make
                                           ~variant:Primary
                                           [ txt "Primary" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Secondary
                                           [ txt "Secondary" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Destructive
                                           [ txt "Destructive" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Primary_outline
                                           [ txt "Primary outline" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Secondary_outline
                                           [ txt "Secondary outline" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Destructive_outline
                                           [ txt "Destructive outline" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Ghost
                                           [ txt "Ghost" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~variant:Twitch
                                           [ txt "Twitch" ]
                                       ]
                                   ]
                               ]
                           ; div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Button sizes" ]
                               ; ul
                                   ~a:
                                     [ a_class
                                         [ "list-disc"; "ml-2"; "space-y-2" ]
                                     ]
                                   [ li
                                       [ Button.make ~size:Small [ txt "Small" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~size:Medium
                                           ~variant:Secondary
                                           [ txt "Medium" ]
                                       ]
                                   ; li
                                       [ Button.make
                                           ~size:Large
                                           ~variant:Destructive
                                           [ txt "Large" ]
                                       ]
                                   ]
                               ]
                           ]
                       ]
                   ; Card.make
                       ~a:[ a_class [ "w-full" ] ]
                       [ Typography.make
                           ~a:
                             [ a_class
                                 [ "mb-1"
                                 ; "border-b"
                                 ; "border-b-slate-400"
                                 ; "pb-2"
                                 ]
                             ]
                           ~as_elt:H1
                           ~size:Large
                           ~font_weight:Bold
                           [ txt "Tables" ]
                       ; div
                           ~a:
                             [ a_class [ "flex flex-row justify-between p-4" ] ]
                           [ div
                               ~a:[ a_class [ "flex"; "flex-col" ] ]
                               [ Typography.make
                                   ~a:[ a_class [ "mb-2" ] ]
                                   ~as_elt:H2
                                   ~font_weight:Semi_bold
                                   [ txt "Variants" ]
                               ; ul
                                   ~a:
                                     [ a_class
                                         [ "list-disc"; "ml-2"; "space-y-2" ]
                                     ]
                                   [ li [ Typography.make [ txt "Fixed" ] ]
                                   ; li [ Typography.make [ txt "Responsive" ] ]
                                   ; li [ Typography.make [ txt "Unstyled" ] ]
                                   ]
                               ]
                           ]
                       ; div
                           ~a:[ a_class [ "flex flex-row justify-center" ] ]
                           [ Table.make
                               ~thead:
                                 (Table.Head.make
                                    [ Table.Row.make
                                        ~hover_style:None
                                        [ Table.Header_cell.make
                                            [ txt "Article name" ]
                                        ; Table.Header_cell.make
                                            [ txt "Author" ]
                                        ; Table.Header_cell.make
                                            [ txt "Publish date" ]
                                        ]
                                    ])
                               [ Table.Body.make
                                   [ Table.Row.make
                                       [ Table.Data_cell.make
                                           [ txt
                                               "OCamlin' through Functional \
                                                Forests"
                                           ]
                                       ; Table.Data_cell.make
                                           [ txt "Jamie Lisper" ]
                                       ; Table.Data_cell.make
                                           [ txt "2023-04-15" ]
                                       ]
                                   ; Table.Row.make
                                       [ Table.Data_cell.make
                                           [ txt
                                               "HTMX Marks the Spot: \
                                                Interactive Web Without the \
                                                Wait"
                                           ]
                                       ; Table.Data_cell.make
                                           [ txt "Alex Render" ]
                                       ; Table.Data_cell.make
                                           [ txt "2023-06-12" ]
                                       ]
                                   ; Table.Row.make
                                       [ Table.Data_cell.make
                                           [ txt
                                               "Tailwind Blowing Through My \
                                                CSS Laundry"
                                           ]
                                       ; Table.Data_cell.make
                                           [ txt "Pat Styles" ]
                                       ; Table.Data_cell.make
                                           [ txt "2023-08-01" ]
                                       ]
                                   ; Table.Row.make
                                       [ Table.Data_cell.make
                                           [ txt
                                               "Monads & Monarchies: Kingdoms \
                                                of Functional Realms"
                                           ]
                                       ; Table.Data_cell.make
                                           [ txt "Sam Haskell" ]
                                       ; Table.Data_cell.make
                                           [ txt "2023-10-05" ]
                                       ]
                                   ; Table.Row.make
                                       [ Table.Data_cell.make
                                           [ txt
                                               "Lambda and the Search for the \
                                                Infinite Loop"
                                           ]
                                       ; Table.Data_cell.make
                                           [ txt "Taylor Recursion" ]
                                       ; Table.Data_cell.make
                                           [ txt "2023-09-22" ]
                                       ]
                                   ]
                               ]
                           ]
                       ]
                   ]
               ]
           ]
       ])
  |> Util.html_to_string
;;

let () =
  Fmt.pr "[component_library_playground] starting dream";
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.html @@ index ~theme:Dark ())
       ; Dream.post "/toggle-theme" (fun request ->
           let theme =
             Dream.query request "theme"
             |> Option.get
             |> Toggle_theme_button.theme_of_string
           in
           Dream.html @@ index ~theme ())
       ; Dream_livereload.route ()
       ; Dream.get "/static/**" (Dream.static "static/")
       ]
;;
