(** Big enough integers  *)

include module type of Int64

val two : t

val equal : t -> t -> bool

val le : t -> t -> bool

val lt : t -> t -> bool

val min : t -> t -> t

val max : t -> t -> t

val to_int64 : t -> int64

val of_int64 : int64 -> t
