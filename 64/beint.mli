(** Big enough integers (version for 64-bit platforms) *)

(** A Beint integer is at least 63-bit and it avoids boxing on 64-bit platforms. *)

type t

val zero : t

val one : t

val two : t

external neg : t -> t = "%negint"

external add : t -> t -> t = "%addint"

external sub : t -> t -> t = "%subint"

external mul : t -> t -> t = "%mulint"

external div : t -> t -> t = "%divint"

(** Integer remainder.  If [y] is not zero, the result of [Beint.rem x y] satisfies the
    following property: [x = Beint.add (Beint.mul (Beint.div x y) y) (Beint.rem x y)].  If
    [y = 0], [Beint.rem x y] raises [Division_by_zero]. *)
external rem : t -> t -> t = "%modint"

external succ : t -> t = "%succint"

external pred : t -> t = "%predint"

(** Greatest representable 63-bit integer: 2{^63} - 1. *)
val max_int : t

(** Smallest representable 63-bit integer: -2{^63}. *)
val min_int : t

(** [Int64.shift_right x y] shifts [x] to the right by [y] bits. This is an arithmetic
    shift: the sign bit of [x] is replicated and inserted in the vacated bits.  The result
    is unspecified if [y < 0] or [y >= 64]. **)
external shift_right : t -> int -> t = "%lsrint"

external of_int : int -> t = "%identity"

external to_int : t -> int = "%identity"

(** Represent a 64-bit integer in decimal. *)
val to_string : t -> string

external equal : t -> t -> bool = "%equal"

external compare : t -> t -> int = "caml_int_compare"

external lt : t -> t -> bool = "%lessthan"

external le : t -> t -> bool = "%lessequal"

val min : t -> t -> t

val max : t -> t -> t

val of_int64 : int64 -> t

val to_int64 : t -> int64
