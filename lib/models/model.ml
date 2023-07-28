module type S = sig
  type t [@@deriving yojson, show { with_path = false }]

  val name : string
end

module Category = struct
  type t =
    | Video
    | Article
    | Website
    | Twitch
  [@@deriving enumerate, yojson, show { with_path = false }]

  let name = "category"
end
