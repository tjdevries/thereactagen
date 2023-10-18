open Tyxml.Html
open Components

(* TODO: Split this out into the component-library lib w/ better helpers + abstractions *)
let index () =
  html
    (head
       (title (txt "Component Library Playground"))
       [ script ~a:[ a_src "https://cdn.tailwindcss.com" ] (txt "") ])
    (body
       ~a:[ a_class [ "dark:bg-slate-900"; "dark:text-slate-400" ] ]
       [ div
           ~a:[ a_class [ "w-screen"; "h-screen"; "dark" ] ]
           [ div
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
                       ~a:[ a_class [ "flex flex-row justify-between p-4" ] ]
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
                                   [ Typography.make ~size:Small [ txt "Small" ]
                                   ]
                               ; li
                                   [ Typography.make
                                       ~size:Medium
                                       [ txt "Medium" ]
                                   ]
                               ; li
                                   [ Typography.make ~size:Large [ txt "Large" ]
                                   ]
                               ]
                           ]
                       ]
                   ; div
                       ~a:[ a_class [ "flex flex-row justify-between p-4" ] ]
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
                       ~a:[ a_class [ "flex flex-row justify-between p-4" ] ]
                       [ div
                           ~a:[ a_class [ "flex"; "flex-col" ] ]
                           [ Typography.make
                               ~a:[ a_class [ "mb-2" ] ]
                               ~as_elt:H2
                               ~font_weight:Semi_bold
                               [ txt "Variants" ]
                           ; ul
                               ~a:
                                 [ a_class [ "list-disc"; "ml-2"; "space-y-2" ]
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
                                   [ Button.make ~variant:Ghost [ txt "Ghost" ]
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
                                 [ a_class [ "list-disc"; "ml-2"; "space-y-2" ]
                                 ]
                               [ li [ Button.make ~size:Small [ txt "Small" ] ]
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
                       ~a:[ a_class [ "flex flex-row justify-between p-4" ] ]
                       [ div
                           ~a:[ a_class [ "flex"; "flex-col" ] ]
                           [ Typography.make
                               ~a:[ a_class [ "mb-2" ] ]
                               ~as_elt:H2
                               ~font_weight:Semi_bold
                               [ txt "Variants" ]
                           ; ul
                               ~a:
                                 [ a_class [ "list-disc"; "ml-2"; "space-y-2" ]
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
                                    ; Table.Header_cell.make [ txt "Author" ]
                                    ; Table.Header_cell.make
                                        [ txt "Publish date" ]
                                    ]
                                ])
                           [ Table.Body.make
                               [ Table.Row.make
                                   [ Table.Data_cell.make
                                       [ txt
                                           "OCamlin' through Functional Forests"
                                       ]
                                   ; Table.Data_cell.make [ txt "Jamie Lisper" ]
                                   ; Table.Data_cell.make [ txt "2023-04-15" ]
                                   ]
                               ; Table.Row.make
                                   [ Table.Data_cell.make
                                       [ txt
                                           "HTMX Marks the Spot: Interactive \
                                            Web Without the Wait"
                                       ]
                                   ; Table.Data_cell.make [ txt "Alex Render" ]
                                   ; Table.Data_cell.make [ txt "2023-06-12" ]
                                   ]
                               ; Table.Row.make
                                   [ Table.Data_cell.make
                                       [ txt
                                           "Tailwind Blowing Through My CSS \
                                            Laundry"
                                       ]
                                   ; Table.Data_cell.make [ txt "Pat Styles" ]
                                   ; Table.Data_cell.make [ txt "2023-08-01" ]
                                   ]
                               ; Table.Row.make
                                   [ Table.Data_cell.make
                                       [ txt
                                           "Monads & Monarchies: Kingdoms of \
                                            Functional Realms"
                                       ]
                                   ; Table.Data_cell.make [ txt "Sam Haskell" ]
                                   ; Table.Data_cell.make [ txt "2023-10-05" ]
                                   ]
                               ; Table.Row.make
                                   [ Table.Data_cell.make
                                       [ txt
                                           "Lambda and the Search for the \
                                            Infinite Loop"
                                       ]
                                   ; Table.Data_cell.make
                                       [ txt "Taylor Recursion" ]
                                   ; Table.Data_cell.make [ txt "2023-09-22" ]
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
       [ Dream.get "/" (fun _ -> Dream.html @@ index ())
       ; Dream_livereload.route ()
       ]
;;
