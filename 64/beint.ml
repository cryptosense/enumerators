let () = assert (Sys.word_size = 64)

type t = int

let zero = 0

let one = 1

let two = 2

external neg : int -> int = "%negint"

external add : int -> int -> int = "%addint"

external sub : int -> int -> int = "%subint"

external mul : int -> int -> int = "%mulint"

external div : int -> int -> int = "%divint"

external rem : int -> int -> int = "%modint"

external succ : int -> int = "%succint"

external pred : int -> int = "%predint"

let abs = abs

let max_int = max_int

let min_int = min_int

external shift_right : int -> int -> int = "%lsrint"

external of_int : int -> int = "%identity"

external to_int : int -> int = "%identity"

let to_string = string_of_int

external equal : int -> int -> bool = "%equal"

external compare : int -> int -> int = "caml_int_compare"

external lt : int -> int -> bool = "%lessthan"

external le : int -> int -> bool = "%lessequal"

let min x y = if lt x y then x else y

let max x y = if lt y x then x else y

let of_int64 = Int64.to_int

let to_int64 = Int64.of_int
