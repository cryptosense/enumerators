(** Finite lazy enumerators.

    Enumerators are a memory-efficient way of manipulating finite sequences. *)

type 'a t

(** Raised when an index is out of the bounds of an enumerator. *)
exception Out_of_bounds

(** Retrieve the element at a given index. *)
val nth : 'a t -> int64 -> 'a

(** Get all the elements of an enumeration in order. *)
val elements : 'a t -> 'a list

(** Get the number of elements of an enumeration. *)
val size : 'a t -> int64

(** Same as [Int64.to_int (size e)]. May overflow. *)
val size_int : 'a t -> int

(** Test whether an enumeration is empty. *)
val is_empty : 'a t -> bool

(** [fold_left f acc a] computes [f (... (f (f acc a.(0)) a.(1)) ...)
    a.(n-1)], where n is the size of the enumerator [a]. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Map a function over an enumerator. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Apply a function to the elements of an enumerator. *)
val iter : ('a -> unit) -> 'a t -> unit

(** Allocate an array to hold intermediate values. *)
val memoize : 'a t -> 'a t

(** {2 Constructors} *)

(** Enumerate the elements of the list, in the order defined by the list. *)
val make : 'a list -> 'a t

(** Same as [make]. *)
val of_list : 'a list -> 'a t

type ('t, 'elt) set = (module Set.S with type t = 't and type elt = 'elt)

(** Build an enumerator from a set. *)
val of_set : ('t, 'elt) set -> 't -> 'elt t

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

(** {2 Transformations} *)

(** [firstn n e] enumerates the first [n] elements from the enumerator [e].  If the
    enumerator has less than [n] elements, it will only enumerate those. *)
val firstn : int64 -> 'a t -> 'a list * 'a t

(** Apply a filter on an enumerator.  Warning:  this combinator evaluates its
    elements. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [partition p e] returns a pair of enumerators [(e1, e2)], where [e1] is the enumerator
    of all the elements of [e] that satisfy the predicate [p], and [e2] is the enumerator
    of all the elements of [e] that do not satisfy [p].  The order of the elements from
    the input enumerator is preserved.  Evaluates its elements. *)
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** Enumerate over a random permutation of an existing enumerator.  Warning: This can take
    a lot of time on large enumerators. *)
val shuffle : 'a t -> 'a t

(** [scalar_left a e] enumerates [(a, e0), (a, e1), ...] where [e0, e1, ...] are the
    elements of the enumerator [e]. *)
val scalar_left : 'a -> 'b t -> ('a * 'b) t

(** [scalar_right e b] enumerates [(e0, b), (e1, b), ...] where [e0, e1, ...] are the
    elements of the enumerator [e]. *)
val scalar_right : 'a t -> 'b -> ('a * 'b) t

(** [bitset n] enumerates a bitset of size [n] represented as integers.  [bitset ~k n]
    enumerates the elements having at most [k] ones in their binary representation.

    Elements with fewer ones come first in the enumeration.  For example: [bitset ~k:2 3]
    returns [\[0b000; 0b001; 0b010; 0b100; 0b011; 0b101; 0b110\]].

    [n] must be less than [Sys.word_size - 2] (e.g., 30 or 62, depending on your
    architecture). *)
val bitset : ?k:int -> int -> int t

(** {2 Combinators} *)

(** [append a b] enumerates all the elements of [a] then all the
    elements of [b]. *)
val append : 'a t -> 'a t -> 'a t

(** Interleave two enumerators until one of them becomes empty and then
    append the remaining one after that. *)
val interleave : 'a t -> 'a t -> 'a t

(** Enumerate pairs. *)
val product : 'a t -> 'b t -> ('a * 'b) t

(** Concatenate an enumerator of enumerators.  *)
val squash : 'a t t -> 'a t

(** Squash enumerators in round-robin order.  *)
val round_robin : 'a t t -> 'a t

(** Enumerate balanced subsets.

    This enumerates groups (as enumerators) of tuples (as lists).  Each list contains at
    most one element from each of the input enumerators.

    For instance, to enumerate subsets for [\[1; 2\] and [\[3; 4\]], [subset] enumerates
    four groups.  The first group enumerates the empty list, the second group enumerates
    the singletons from [\[1; 2\]], the third group enumerates singletons from [\[3; 4\]]
    and the fourth group enumerates the pairs with the first element from [\[1; 2\]] and
    the second element from [\[3; 4\]].

    If the argument [k] is supplied, lists longer than [k] will be ignored.

    The order in the resulting lists is the same as in the input enumerator list. For
    example, an element of [a] will never appear after an element of [b] in the lists of
    [subset a b].

    If you do not need the grouping, you can apply [squash] or [round_robin] to the
    result to get an enumerator of lists. *)
val subset : ?k:int -> 'a t list -> 'a list t t

(** Generate all sets of [k] elements by picking at most one element per input list.

    Sets are grouped (as enumerators) by the subset of enumerators from which the
    selection is made.  See [subset] for more information on how grouping is done.

    The argument [k] must be greater than zero. *)
val choose_k_from_list : k:int -> 'a t list -> 'a list t t

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

(** Return the depth of an enumerator, that is, the number of combinators that this
    enumerator is based on.  It is reset when a combinator that evaluates the elements of
    an enumerator is applied. *)
val depth : 'a t -> int
