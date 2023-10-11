(lang dune 3.8)

; opam install dream dream-livereload tyxml bos petrol base fmt lwt caqti caqti-lwt caqti-dynload caqti-driver-sqlite3

(name reactagen)

(generate_opam_files true)

(source (github tjdevries/reactagen))
(authors "TJ DeVries")
(maintainers "TJ DeVries")
(license MIT)

; (documentation https://url/to/documentation)

(package
 (name reactagen)
 (synopsis "A way we can not use reddit")
 (description "TheReactagen is where we host all the infos we need for managing react submissions")
 (depends
   (ocaml (>= "5.0"))
   (dune (>= "3.8.2"))
   (alcotest :with-test)
   dream dream-livereload
   tyxml
   bos
   dotenv
   petrol
   base fmt ppx_let ppx_enumerate ppxlib ppx_deriving ppx_deriving_yojson
   lwt caqti caqti-lwt caqti-dynload caqti-driver-sqlite3
   cohttp-lwt-unix)

 (tags
  (topics "to describe" your project)))

(package
  (name shoelace)
  (synopsis "Shoelace bindings")
  (depends
    (ocaml (>= "5.0"))
    (dune (>= "3.8.2"))
    yojson ppx_deriving_yojson
    base fmt tyxml))

(package
  (name twitch)
  (synopsis "Twitch API Bindings")
  (depends
   (ocaml (>= "5.0"))
   (dune (>= "3.8.2"))
   base fmt
   lwt cohttp-lwt-unix
   yojson ppx_deriving ppx_deriving_yojson))


(package
 (name ppx_combust)
 (synopsis "Generate boilerplate stuff for petrol to go zoom zoom")
 (depends ocaml ppxlib))
