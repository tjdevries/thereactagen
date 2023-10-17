open Tyxml.Html
open Components

let index () =
  html
    (head
       (title (txt "Component Library Playground"))
       [ script ~a:[ a_src "https://cdn.tailwindcss.com" ] (txt "") ])
    (body
       [ div
           ~a:[ a_class [ "w-screen"; "h-screen"; "dark" ] ]
           [ div
               ~a:
                 [ a_class
                     [ "w-full"
                     ; "h-full"
                     ; "flex"
                     ; "flex-col"
                     ; "items-center"
                     ; "dark:bg-slate-900"
                     ]
                 ]
               [ div
                   ~a:
                     [ a_class
                         [ "w-full"
                         ; "grid"
                         ; "grid-cols-2"
                         ; "justify-items-center"
                         ; "gap-4"
                         ; "p-8"
                         ]
                     ]
                   [ Card.make
                       ~classes:[ "w-full" ]
                       [ Typography.make
                           ~as_elt:H1
                           ~size:Large
                           [ txt "Typography" ]
                       ; div
                           ~a:[ a_class [ "flex"; "flex-col" ] ]
                           [ Typography.make
                               ~classes:[ "mb-2" ]
                               ~as_elt:H2
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
                       ]
                   ; Card.make
                       ~classes:[ "w-full" ]
                       [ Typography.make
                           ~as_elt:H1
                           ~size:Large
                           [ txt "Buttons" ]
                       ]
                   ; Card.make
                       ~classes:[ "w-full" ]
                       [ Typography.make ~as_elt:H1 ~size:Large [ txt "Tables" ]
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
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html @@ index ()) ]
;;
