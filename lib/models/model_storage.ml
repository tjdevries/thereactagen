type _ storage =
  | Int : int storage
  | String : string storage

let to_caqti_storage : type a. a storage -> a Caqti_type.t = function
  | Int -> Caqti_type.int
  | String -> Caqti_type.string
;;

module Storage = struct
  module type S = sig
    type s

    val storage : s storage
  end

  module IntStorage : S with type s = int = struct
    type s = int

    let storage = Int
  end

  module StringStorage : S with type s = string = struct
    type s = string

    let storage = String
  end
end

module type S = sig
  include Model.S
  include Storage.S

  val encode : t -> (s, string) result
  val decode : s -> (t, string) result
end

module Make (M : S) = struct
  include M

  let ty = Caqti_type.custom ~encode ~decode (to_caqti_storage storage)
  let petrol_type = Petrol.Type.custom ~ty ~repr:name
end

module MakeString (M : Model.S) = struct
  include Make (struct
    include M
    include Storage.StringStorage

    let encode t = Ok (to_yojson t |> Yojson.Safe.to_string)
    let decode s = Yojson.Safe.from_string s |> of_yojson
  end)
end
