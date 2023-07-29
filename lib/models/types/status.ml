module M = struct
  type t =
    | Submitted
    | Reviewed
    | Pending
    | Rejected
    | Complete
  [@@deriving enumerate, yojson, show { with_path = false }]

  let name = "status"
end

include M
include Storage.Custom.MakeJSON (M)

let resolved = [ Rejected; Complete ]
