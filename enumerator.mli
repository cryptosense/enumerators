(** Finite lazy enumerators.

    Enumerators are a memory-efficient way of manipulating finite sequences. *)

type 'a t

(** Retrieve the element at a given index. *)
val nth : 'a t -> int64 -> 'a

(** Get all the elements of an enumeration. Order is preserved. *)
val elements : 'a t -> 'a list

(** Get the number of elements of an enumeration. *)
val cardinal : 'a t -> int64

(** Same as [Beint.to_int cardinal e]. May overflow. *)
val size : 'a t -> int

(** Test whether an enumeration is empty. *)
val is_empty : 'a t -> bool

(** [fold_left f acc a] computes [f (... (f (f acc a.(0)) a.(1)) ...)
    a.(n-1)], where n is the cardinal of the enumerator [a]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Apply a function to the elements of an enumerator. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Allocate an array to hold intermediate values. *)
val memoize : 'a t -> 'a t

(** {2 Constructors} *)

(** Enumerate the elements of the list, in the order defined by the list. *)
val make : 'a list -> 'a t

(** Same as [make]. *)
val of_list : 'a list -> 'a t

(** Empty enumerator. *)
val empty : 'a t

(** Singleton enumerator. *)
val constant : 'a -> 'a t

(** Enumerate a singleton which shall be recomputed each time it is
    enumerated. *)
val constant_delayed : (unit -> 'a) -> 'a t

(** [range a b] produces an enumerator for the integers between [a]
    and [b] included. If [b < a] the range is empty. *)
val range : int -> int -> int t

(** Enumerate pairs. *)
val product : 'a t -> 'b t -> ('a * 'b) t

val scalar_left : 'a -> 'b t -> ('a * 'b) t
val scalar_right : 'a t -> 'b -> ('a * 'b) t

(** Map a function over an enumerator. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Squash enumerators.  *)
val squash : 'a t t -> 'a t

(** Alternative implementation of [Enumerator.squash] using [Enumerator.append], faster
    here. *)
val squash_append : 'a t t -> 'a t

(** Squash enumerators in a round-robin fashion.  *)
val round_robin : 'a t t -> 'a t

(** Apply a filter on an enumerator.  Warning:  this combinator evaluates its
    elements. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Interleave two enumerators until one of them becomes empty and then
    append the remaining one after that. *)
val interleave : 'a t -> 'a t -> 'a t

(** [append a b] enumerates all the elements of [a] then all the
    elements of [b]. *)
val append : 'a t -> 'a t -> 'a t

(** [partition p e] returns a pair of enumerators [(e1, e2)], where [e1] is the enumerator
    of all the elements of [e] that satisfy the predicate [p], and [e2] is the enumerator
    of all the elements of [e] that do not satisfy [p].  The order of the elements from
    the input enumerator is preserved.  Warning: this combinator evaluates its elements. *)
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** [firstn n e] extracts [n] elements from the enumerator [e].  If the enumerator has
    less than [n] elements, it will only extract those. *)
val firstn : int64 -> 'a t -> 'a list * 'a t

(** Enumerates a bitset of size [n] represented as integers. [n] must
    be less than [Sys.word_size - 2] (e.g., 30 or 62, depending on
    your architecture). The bitset is enumerated by levels, that is,
    all subsets of size k are enumerated, then all subsets of size k+1
    and so on. *)
val bitset : ?k:int -> int -> int t

(** Make a balanced subset enumerator.

    The resulting enumerator has type ['a list t t] (that is, is an
    enumerator of enumerators of lists of 'a). Each ['a list t]
    correspond to a group of subsets. The first group of subset
    contains the subset in which the lists that contain one item; the
    second group of subset contains the lists that contain two items;
    and so on.

    In particular, notice that this does not yield an enumerator of
    the lists in lexicographic order.

    The user must then use either [squash] or [round_robin] to
    collapse the two levels of [t] into one.

    The order of the list of enumerators given in arguments is
    preserved in the result. That is, given [a] and [b] two
    enumerators, the elements of [a] will always appear before the
    elemetns of [b]  in the result of [subset [a; b]] *)
val subset : ?k:int -> 'a t list -> 'a list t t

(** Generate all sets of k elements by picking at most one element per input list. *)
val choose_k_from_list : k:int -> 'a t list -> 'a list t t

type ('t, 'elt) set = (module Set.S with type t = 't and type elt = 'elt)

val of_set : ('t, 'elt) set -> 't -> 'elt t

(** Enumerate over a random permutation of an existing enumerator.  Warning: This can take
    a lot of time on large enumerators. *)
val shuffle : 'a t -> 'a t

(** [maybe f e] builds the enumerator of [x; f x] for [x] in [e].  *)
val maybe : ('a -> 'a) -> 'a t -> 'a t

(** [maybe_cons h e] builds the enumerator of [x; h::x] for [x] in
    [e] *)
val maybe_cons : 'a -> 'a list t -> 'a list t

(** [maybe_some_of k l e] builds the subsets of size [k] from [l] and
    for each such subset [s1] and each [s2] in [e], builds [s1 @
    s2].  *)
val maybe_some_of : k:int -> 'a list -> 'a list t -> 'a list t


(** {2 Debug}  *)

val depth : 'a t -> int
