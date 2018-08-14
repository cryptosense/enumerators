include Int64

let two = 2L

external lt : int64 -> int64 -> bool = "%lessthan"

external le : int64 -> int64 -> bool = "%lessequal"

external equal : int64 -> int64 -> bool = "%equal"

let of_int64 i = i

let to_int64 i = i

let min x y = if lt x y then x else y

let max x y = if lt y x then x else y
