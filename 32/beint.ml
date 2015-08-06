let () = assert (Sys.word_size = 32)

include Int64

let two = 2L

external lt : int64 -> int64 -> bool = "%lessthan"

external le : int64 -> int64 -> bool = "%lessequal"

external gt : int64 -> int64 -> bool = "%greaterthan"

external ge : int64 -> int64 -> bool = "%greaterequal"

external equal : int64 -> int64 -> bool = "%equal"

external not_equal : int64 -> int64 -> bool = "%notequal"

let of_int64 i = i

let to_int64 i = i

let min x y = if lt x y then x else y

let max x y = if lt y x then x else y
