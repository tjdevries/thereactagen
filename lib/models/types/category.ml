open Base

module M = struct
  type t =
    | Video
    | Article
    | Website
    | News
    | Meme
  [@@deriving enumerate, yojson, show { with_path = false }]

  let name = "category"
end

include M
include Storage.Custom.MakeJSON (M)

let make_select ~name () =
  let open Tyxml.Html in
  let options =
    List.map all ~f:(fun t ->
      option ~a:[ a_value (encode t |> Result.ok_or_failwith) ] (txt @@ show t))
  in
  select
    ~a:[ a_class [ "select"; "select-bordered" ]; a_id name; a_name name ]
    options
;;
