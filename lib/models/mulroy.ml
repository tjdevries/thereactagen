(* TODO: Think about if we can make this simpler
   for example, maybe we have some StringStorage or something
   so that it's not so easy to get weird unhelpful errors *)
(* module type Storage = sig *)
(*   type storage *)
(*   val caqti_storage : storage Caqti_type.t *)
(* end *)

module type S = sig
  type t
  type storage

  val caqti_storage : storage Caqti_type.t
  val encode_exn : t -> storage
  val decode_exn : storage -> t

  (** The name to display  *)
  val repr : string
end

module Make (M : S) = struct
  type t = M.t
  type storage = M.storage

  let decode_exn = M.decode_exn
  let encode_exn = M.encode_exn

  let decode t =
    match decode_exn t with
    | decoded -> Ok decoded
    | exception exn -> Error (Printexc.to_string exn)
  ;;

  let encode t =
    match encode_exn t with
    | encoded -> Ok encoded
    | exception exn -> Error (Printexc.to_string exn)
  ;;

  let ty = Caqti_type.(custom ~encode ~decode M.caqti_storage)
  let custom = Petrol.Type.custom ~ty ~repr:M.repr
end
