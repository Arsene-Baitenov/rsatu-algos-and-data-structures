module Tree : functor
    (K : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val pp : Format.formatter -> t -> unit
       val show : t -> string
       val to_yojson : t -> Yojson.Safe.t
       val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
     end)
    (V : sig
       type t

       val pp : Format.formatter -> t -> unit
       val show : t -> string
       val to_yojson : t -> Yojson.Safe.t
       val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
     end)
    -> sig
  type data = K.t * V.t

  val pp_data : Format.formatter -> data -> unit
  val show_data : data -> string
  val data_to_yojson : data -> Yojson.Safe.t
  val data_of_yojson : Yojson.Safe.t -> data Ppx_deriving_yojson_runtime.error_or

  type t =
    | StubLeaf
    | Node of data * int * t * t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val empty : t
  val find : t -> K.t -> V.t option
  val insert : t -> K.t -> V.t -> t
  val remove : t -> K.t -> t
  val to_json_string : t -> string
  val of_json_string : string -> t Ppx_deriving_yojson_runtime.error_or
end
