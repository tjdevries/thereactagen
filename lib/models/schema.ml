open Petrol

module type DB = Caqti_lwt.CONNECTION

(* define a new schema *)
let schema = StaticSchema.init ()
