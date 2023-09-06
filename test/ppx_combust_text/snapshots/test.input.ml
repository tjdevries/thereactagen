type t =
  { id : int [@primary { auto_increment = true }]
  ; username : string [@unique]
  ; display_name : string
  ; password : string
  }
[@@deriving combust ~name:"users"]
