type t =
  { id : int [@primary]
  ; username : string [@unique]
  ; display_name : string
  ; password : string
  }
[@@deriving combust ~name:"users"]
