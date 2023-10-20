type t =
  | Dark
  | Light

let to_string = function
  | Dark -> "dark"
  | Light -> "light"
;;

let of_string_opt = function
  | "dark" -> Some Dark
  | "light" -> Some Light
  | _ -> None
;;

module Color = struct
  (* TODO : Hide behind mli *)
  (* type t = (color * weight) *)
  type t = string * int

  let to_weight color = function
    | 50 -> Ok (fst color, 50)
    | 100 -> Ok (fst color, 100)
    | 200 -> Ok (fst color, 200)
    | 300 -> Ok (fst color, 300)
    | 400 -> Ok (fst color, 400)
    | 500 -> Ok (fst color, 500)
    | 600 -> Ok (fst color, 600)
    | 700 -> Ok (fst color, 700)
    | 800 -> Ok (fst color, 800)
    | 900 -> Ok (fst color, 900)
    | 950 -> Ok (fst color, 950)
    | _ -> Error (`Msg "Invalid weight")
  ;;
end
