type t =
  { id : int [@primary]
  ; username : string option
  }
[@@deriving combust ~name:"optional"]
