type _ storage =
  | Int : int storage
  | String : string storage

module Storage : sig
  module type S = sig
    type s

    val storage : s storage
  end

  module IntStorage : sig
    type s = int

    val storage : s storage
  end

  module StringStorage : sig
    type s = string

    val storage : s storage
  end
end

module type S = sig
  include Model.S
  include Storage.S

  val encode : t -> (s, string) result
  val decode : s -> (t, string) result
end

(* TODO: How can I make this work? *)
(* I want to both of the Makes to do this *)
(* module type CustomStorage = sig *)
(*   type t *)
(*   type s *)
(*   val encode : t -> (s, string) result *)
(*   val decode : s -> (t, string) result *)
(*   val petrol_type : t Petrol.Type.t *)
(* end *)

module Make (M : S) : sig
  type t = M.t
  type s = M.s

  val encode : t -> (s, string) result
  val decode : s -> (t, string) result
  val petrol_type : t Petrol.Type.t
end

module MakeJSON (M : Model.S) : sig
  type t = M.t
  val show : t -> string

  type s = string

  val encode : t -> (s, string) result
  val decode : s -> (t, string) result
  val petrol_type : t Petrol.Type.t
end
