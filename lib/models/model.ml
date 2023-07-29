module type S = sig
  type t [@@deriving yojson, show { with_path = false }]

  val name : string
end
