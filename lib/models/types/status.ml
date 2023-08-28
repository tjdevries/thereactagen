module M = struct
  type t =
    | Submitted
    | Pending
    | Rejected
    | Complete
  [@@deriving enumerate, yojson, show { with_path = false }]

  let name = "status"
end

include Storage.Custom.MakeJSON (M)
include M

let resolved = [ Rejected; Complete ]
