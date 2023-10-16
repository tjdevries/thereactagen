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
                     ; "py-8"
                     ; "dark:bg-slate-900"
                     ]
                 ]
               [ div
                   ~a:[ a_class [ "w-2/3" ] ]
                   [ Card.make
                       [ Typography.make
                           ~as_elt:H1
                           ~size:Large
                           [ txt "Hello, world" ]
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
