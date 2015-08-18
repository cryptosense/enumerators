type 'a t =
  {
    size : Beint.t;
    nth : Beint.t -> 'a;
    shape : string; (* how the enumerator was created, useful for debugging *)
    depth : int (* number of composed functions to create values, useful for debugging *)
  }

exception Out_of_bounds

let nth s i =
  let i = Beint.of_int64 i in
  if Beint.lt i s.size && Beint.(le zero i)
  then s.nth i
  else raise Out_of_bounds

let is_empty s =
  Beint.equal Beint.zero s.size

let size s =
  Beint.to_int64 s.size

let size_int s =
  Beint.to_int s.size

let fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (e : 'b t) : 'a =
  let rec aux i acc =
    if Beint.equal i e.size
    then acc
    else aux (Beint.succ i) (f acc (e.nth i))
  in
  aux Beint.zero acc

let map f e =
  let e_nth = e.nth in
  let nth i = f (e_nth i) in
  {
    size = e.size;
    nth;
    shape = "map";
    depth = e.depth + 1;
  }

let iteri (f : int64 -> 'a -> unit) (e : 'a t) : unit =
  let  rec aux i =
    if not (Beint.equal i e.size)
    then
      begin
        f (Beint.to_int64 i) (e.nth i);
        aux (Beint.succ i)
      end
  in
  aux Beint.zero

let iter f a = iteri (fun _ x -> f x) a

let max_i (x : int) y = if x < y then y else x

let min_i (x : int) y = if x < y then x else y

(** [is_int i] tests whether [i] can be safely represented as a value of type [int] *)
let is_int =
  let n = Beint.of_int max_int in
  let m = Beint.of_int min_int in
  fun (i : Beint.t) -> Beint.le i n && Beint.le m i

let of_array (v : 'a array) : 'a t =
  let size = Beint.of_int (Array.length v) in
  let nth i =
    let i = Beint.to_int i in
    Array.get v i
  in
  { size; nth; shape = "of_array"; depth = 0 }

let make l =
  let v = Array.of_list l in
  of_array v

let of_list = make

let empty =
  let nth _ = raise Out_of_bounds in
  {
    size = Beint.zero;
    nth;
    shape = "empty";
    depth = 0
  }

let constant e =
  let nth i =
    assert (Beint.equal Beint.zero i);
    e
  in
  {size = Beint.one;
   nth;
   shape = "constant";
   depth = 0}

let constant_delayed e =
  let nth i =
    assert (Beint.equal Beint.zero i);
    e ()
  in
  {size = Beint.one;
   nth;
   shape = "constant_delayed";
   depth = 0}

let memoize e =
  Array.init (Beint.to_int e.size) (fun i -> Beint.of_int i |> e.nth)
  |> of_array

(******************************************************************************)
(*                         Operations on Enumerations                         *)
(******************************************************************************)

let filter (f : 'a -> bool) (e : 'a t) : 'a t =
  let rec indices acc i =
    if Beint.equal i e.size
    then List.rev acc
    else let elt = e.nth i in
      if f elt
      then indices (elt :: acc) (Beint.succ i)
      else indices acc (Beint.succ i)
  in
  let indices = (indices [] Beint.zero) in
  let indices = Array.of_list indices in
  let enum = of_array indices in
  {enum with shape = "filter"}

let append e1 e2 =
  if is_empty e1 then
    e2 (* optimization *)
  else if is_empty e2 then
    e1 (* optimization *)
  else
    let e1_size = e1.size in
    let e1_nth = e1.nth in
    let e2_size = e2.size in
    let e2_nth = e2.nth in
    let nth i =
      if Beint.lt i e1_size then
        e1_nth i
      else
        e2_nth (Beint.sub i e1_size) in
    {
      size = Beint.add e1_size e2_size;
      nth;
      shape = "append";
      depth = max_i e1.depth e2.depth + 1
    }

let sub s start len =
  if Beint.lt start Beint.zero ||
     Beint.lt len Beint.zero ||
     Beint.lt s.size (Beint.add start len)
  then
    invalid_arg
      (Printf.sprintf "sub (start:%s) (len%s) (size:%s)"
         (Beint.to_string start)
         (Beint.to_string len)
         (Beint.to_string s.size))
  else
    let size = len in
    let s_nth = s.nth in
    let nth i = s_nth (Beint.add start i) in
    {
      size;
      nth;
      shape = "sub";
      depth = 1 + s.depth
    }

(* Interleaving two enumerators of the same size. *)
let interleave' e1 e2 =
  assert (Beint.equal e1.size e2.size);
  let e1_nth = e1.nth in
  let e2_nth = e2.nth in
  let nth i =
    if Beint.(equal zero (rem i two))
    then e1_nth (Beint.shift_right i 1)
    else e2_nth (Beint.shift_right i 1)
  in
  {size = Beint.add e1.size e2.size;
   nth;
   shape = "interleave";
   depth = 1 + max_i e1.depth e2.depth
  }

let interleave e1 e2 =
  if Beint.lt e1.size e2.size
  then
    begin
      let e = interleave' (e1) (sub e2 Beint.zero e1.size) in
      append e (sub e2 e1.size (Beint.sub e2.size e1.size))
    end
  else if Beint.equal e1.size e2.size
  then interleave' e1 e2
  else                                  (* e1.size > e2.size *)
    let e = interleave' (sub e1 Beint.zero e2.size) e2 in
    append e (sub e1 e2.size (Beint.sub e1.size e2.size))

(* Return an equivalent sequence of enumerators where empty
   enumerators have been removed for optimization. *)
let prune (e : 'a t t) : 'a t t =
  (* First, compute the number of non-empty enumerations in e. *)
  let rec non_empty i acc =
    if Beint.equal i e.size then acc
    else if Beint.equal Beint.zero (e.nth i).size
    then non_empty (Beint.succ i) acc
    else non_empty (Beint.succ i) (Beint.succ acc)
  in
  let non_empty = non_empty Beint.zero Beint.zero in
  if Beint.equal non_empty Beint.zero
  then empty
  else
    begin
      let v = Array.make (Beint.to_int non_empty) empty in
      let rec aux i cursor =
        if not (Beint.equal i e.size)
        then if is_empty (e.nth i)
          then aux (Beint.succ i) cursor
          else
            begin
              Array.set v cursor (e.nth i);
              aux (Beint.succ i) (succ cursor)
            end
      in
      aux Beint.zero 0;
      of_array v
    end

(* Given an enumeration of enumerators, enumerate all values of the
   first enumerator in the enumeration, then all values of the
   second enumerator, and so on. *)
let squash e =
  let e = prune e in
  let size, depth =
    let rec aux i size depth =
      if Beint.equal i e.size
      then size, depth
      else
        let size = Beint.add (e.nth i).size size in
        let depth =  max_i depth (e.nth i).depth in
        aux (Beint.succ i) size depth
    in
    aux Beint.zero Beint.zero min_int
  in
  let e_nth = e.nth in
  let rec nth k i =
    let f = e_nth k in
    let f_size = f.size in
    if Beint.lt i f_size
    then f.nth i
    else nth (Beint.succ k) (Beint.sub i f_size)
  in
  {
    size;
    depth;
    nth = nth Beint.zero;
    shape = "squash";
  }

let cartesian_product (e : 'a t) (f : 'b t) (k : 'a -> 'b -> 'c) : 'c t =
  let e_size = e.size in
  let size = Beint.mul e_size f.size in
  let e_nth = e.nth in
  let f_nth = f.nth in
  let nth i = k (e_nth (Beint.rem i e_size)) (f_nth (Beint.div i e_size)) in
  {
    size;
    nth;
    shape = "prod";
    depth = 1 + max_i e.depth f.depth}

(** [list e] takes as input a list of enumerators and enumerate the
    the cartesian product of these enumerators. Hence, each element
    in the resulting enumerator has the same length. *)
let rec list (e : 'a t list) : 'a list t =
  match e with
  | [] -> constant []
  | t::q -> cartesian_product t (list q) (fun t q -> t :: q)

let partition (f : 'a -> bool) (e : 'a t) : 'a t * 'a t =
  let e_size = e.size in
  let e_nth = e.nth in
  let rec indices ok ko i =
    if Beint.equal i e_size
    then List.rev ok, List.rev ko
    else
      let elt = e_nth i in
      if f elt
      then indices (elt :: ok) ko (Beint.succ i)
      else indices ok (elt :: ko) (Beint.succ i)
  in
  (* Printf.eprintf "partition\n%!"; *)
  let ok, ko = indices [] [] Beint.zero in
  (* Printf.eprintf "partition done\n%!"; *)
  if ok = []
  then empty, e
  else if ko = []
  then e, empty
  else
    let ok = of_array (Array.of_list ok) in
    let ko = of_array (Array.of_list ko) in
    ok, ko

(** {2 Constructors} *)

let product a b =
  if Beint.equal Beint.zero a.size || Beint.equal Beint.zero b.size
  then empty
  else cartesian_product a b (fun a b -> a, b)

let scalar_left : 'a -> 'b t -> ('a * 'b) t = fun k t ->
  {
    size = t.size;
    nth = (fun i -> k, t.nth i);
    shape = "scalar_left";
    depth = succ t.depth
  }

let scalar_right : 'a t -> 'b -> ('a * 'b) t = fun t k ->
  {
    size = t.size;
    nth = (fun i -> t.nth i, k);
    shape = "scalar_right";
    depth = succ t.depth
  }

(******************************************************************************)
(*                                   bitset                                   *)
(******************************************************************************)

module Bitset = struct

  (* Gosper's Hack *)
  let step (element : int) (size : int) =
    begin
      let c = element land (- element) in
      let r = element + c in
      let next = (((r lxor element) lsr 2) / c) lor r in
      if (next land size) <> 0
      then ((next land (size - 1)) lsl 2) lor 3
      else next
    end

end

let from_n_choose_k ~n ~k =
  let element = 1 lsl k - 1 in                 (* k ones *)
  let size = 1 lsl n in
  let rec generate acc set =
    if set < size
    then
      let c = set land ( - set) in
      let r = set + c in
      let next = (((r lxor set) lsr 2) / c) lor r in
      generate (set::acc) next
    else
      List.rev acc
  in
  make (generate [] element)
;;

let print_binary ?(length=Sys.word_size) n =
  let n = ref n in
  Bytes.init length
    (fun _ ->
       let c = if !n mod 2 = 0 then '0' else '1' in
       n := !n / 2;
       c
    )
  |> Bytes.to_string
;;

(** [binomial n k] computes the binomial coefficient, that is the number of
    ways to pick [k] elements among [n]. *)
let rec binomial n k =
  if k > n then 0
  else if k = 0
  then 1
  else if k = n
  then 1
  else
    binomial (n - 1) (k - 1) + binomial (n - 1) k

let bitset ?k n : int t =
  let size =
    match k with
    | None -> 1 lsl n
    | Some k ->
      let r = ref 0 in
      for i = 0 to min_i k n do
        r := !r + binomial n i
      done;
      !r
  in
  (* let size = 1 lsl n in *)
  let v = Array.make size 0 in
  let r = ref 1 in
  let n = 1 lsl n in
  for i = 1 to size - 1 do
    Array.set v i !r;
    r := Bitset.step !r n
  done;
  of_array v


(******************************************************************************)
(*                                 round robin                                *)
(******************************************************************************)

module Round_robin = struct

  (** We want to compute a round-robin enumeration. The problem is to
      find the nth element in this enumeration. This is an easy task
      if all the enumerations have the same length, but more
      complicated otherwise.

      To solve this issue, we decompose this round robin in
      chunks. The first chunk is a round-robin enumeration of
      enumerators that have the same size. The second chunk is a
      round-robin enumeration of enumerators that have the same size,
      and are the remainders of what was not done in the first chunk.

  *)

  (* check that all elements of [e] have size [size] *)
  let equal_size size (e : 'a t t) : bool =
    fold_left (fun acc x -> acc && Beint.equal x.size size) true e

  let round_robin_equal_size size (e : 'a t t) : 'a t =
    assert (equal_size size e);
    let e_nth = e.nth
    and e_size = e.size in
    let nth i =
      (e_nth (Beint.rem i e_size)).nth (Beint.div i e_size)
    in
    {
      size = Beint.mul e.size size;
      depth = 1 + fold_left (fun acc x -> max_i acc x.depth) 0 e;
      shape = "rr";
      nth
    }

  let round_robin (e : 'a t t) : 'a t =
    let rec chunks (e : 'a t t) (acc : 'a t list) =
      let e = prune e in
      if is_empty e
      then
        begin
          List.rev acc
        end
      else
        let min_size = fold_left (fun acc x -> Beint.min acc x.size) Beint.max_int e in
        let chunk = map (fun f -> sub f Beint.zero min_size) e in
        let chunk = round_robin_equal_size min_size chunk in
        let rest = map (fun f -> sub f min_size (Beint.sub f.size min_size)) e in
        chunks rest (chunk::acc)
    in
    let chunks = chunks e [] in
    begin match chunks with
      | [] -> empty
      | [enum] -> enum
      | chunks -> squash (make chunks)
    end

end

let round_robin = Round_robin.round_robin

(******************************************************************************)
(*                                    Subset                                  *)
(******************************************************************************)

module Subset =
struct

  let enum_of_bitmask (g : 'a array) (bits : int) =
    let rec aux i acc=
      if i = Array.length g
      then List.rev acc
      else
      if bits land (1 lsl i) <> 0
      then
        aux (i + 1) (Array.get g i  :: acc)
      else
        aux (i+1) acc
    in
    list (aux 0 [])

  let of_list ?k list =
    match list with
    | [] ->
      empty
    | _ :: _ ->
      let g = Array.of_list list in
      map (enum_of_bitmask g) (bitset ?k (Array.length g))
end

let subset ?k l = Subset.of_list ?k l

let choose_k_from_list ~k l =
  match l with
  | [] -> empty
  | _ :: _ ->
    let k = min k (List.length l) in
    let g = Array.of_list l in
    let n = Array.length g in
    map (Subset.enum_of_bitmask g) (from_n_choose_k ~k ~n)

(** [range a b] produces an enumerator for the integers between [a]
    and [b] included. If [b < a] the range is empty. *)
let range (a : int) (b : int) =
  let size = succ (b - a) in
  let size = max_i 0 size in
  let nth i =
    let i = Beint.to_int i in
    assert (i < size);
    a + i
  in
  let size = Beint.of_int size in
  {size; nth; shape = "range"; depth = 1}

(******************************************************************************)
(*                                    array                                   *)
(******************************************************************************)

(** [array ve] takes as input an array of enumerators and returns an
    enumerator of the cartesian product. This function is much more
    efficient than its [list] equivalent. *)
let array (t : 'a t array) : 'a array t =
  let n = Array.length t in
  let state = Array.make n Beint.zero in
  let nth index =
    let rec aux (i : int) (value : Beint.t) =
      if i = n
      then Array.mapi
          (fun (i : int) (j : Beint.t) ->
             let ti = Array.get t i in
             ti.nth j)
          state
      else
        begin
          let ti = Array.get t i in
          let ti_size = ti.size in
          assert (Beint.lt Beint.zero ti_size);
          Array.set state i (Beint.rem value ti_size);
          aux (succ i) (Beint.div value ti_size)
        end
    in
    aux 0 index
  in
  let size = Array.fold_left (fun acc enum -> Beint.mul acc enum.size) Beint.one t in
  let depth = Array.fold_left (fun acc enum -> max_i acc enum.depth) 0 t in
  {
    size;
    depth;
    shape = "array";
    nth;
  }

type ('t, 'elt) set = (module Set.S with type t = 't and type elt = 'elt)

(* The following version is the most efficient way to build an enum out of a set. I
   experimented with two versions that use Set.fold, but they are much less efficient.  *)
let of_set (type t) (type elt) ((module Set) : (t, elt) set) (t : t) =
  let elements = Set.elements t in
  make elements

let elements s =
  let r = ref [] in
  let i = ref Beint.zero in
  while Beint.lt !i s.size do
    r := (s.nth !i) :: !r;
    i := Beint.succ !i
  done;
  List.rev !r

let firstn len s =
  let len = Beint.of_int64 len in
  if Beint.lt s.size len
  then elements s, empty
  else
    elements (sub s Beint.zero len),
    sub s len (Beint.sub s.size len)

let depth s = s.depth

let shuffle_array arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done

let shuffle e =
  let permutation = Array.init (Beint.to_int e.size) Beint.of_int in
  shuffle_array permutation;
  { size = e.size
  ; nth = (fun i -> e.nth (permutation.(Beint.to_int i)))
  ; shape = "shuffle"
  ; depth = 1 + e.depth
  }

(******************************************************************************)
(*                              Maybe combinators                             *)
(******************************************************************************)

let maybe f e =
  interleave e (map f e)

let maybe_cons (hd : 'a) (e : 'a list t) : 'a list t =
  maybe (fun x -> hd :: x) e

let maybe_some_of ~k (list : 'a list) (e : 'a list t) : 'a list t =
  if list == [] then
    e
  else
    let options = subset ~k (List.map constant list) |> squash in
    map (fun options -> map (fun base -> options @ base) e) options
    |> round_robin
