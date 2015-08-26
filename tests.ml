open OUnit2
open Enumerator

let pp_int = string_of_int

let pp_list pp_elem l =
  "[" ^ String.concat "; " (List.map pp_elem l) ^ "]"

let pp_pair pp_e1 pp_e2 (e1, e2) =
  "(" ^ pp_e1 e1 ^ ", " ^ pp_e2 e2 ^ ")"

let (===) a b =
  fun _ -> assert_equal a b

let map_test test inputs =
  let make do_test (name, output, input) =
    name >:: (fun _ -> do_test output input) in
  List.map (make test) inputs

let one_two = make [1; 2]

let test_elements_make = [
  "empty" >:: ([] === elements (make []));
  "one_two" >:: ([1; 2] === elements one_two);
]

let test_nth = [
  "empty" >:: (fun _ ->
      assert_raises Out_of_bounds (fun () -> nth empty 0L));
  "one_two_first" >:: (1 === nth one_two 0L);
  "one_two_second" >:: (2 === nth one_two 1L);
]

let test_size = [
  "empty" >:: (0L === size empty);
  "empty_int" >:: (0 === size_int empty);
  "one_two" >:: (2L === size one_two);
  "one_two_int" >:: (2 === size_int one_two);
]

let test_is_empty = [
  "empty" >:: (true === is_empty empty);
  "one_two" >:: (false === is_empty one_two);
]

let test_fold_left =
  let cons = fun t h -> h :: t in

  let test output input =
    assert_equal ~printer:(pp_list pp_int) output (fold_left cons [] input) in

  map_test test [
    ("empty", [], empty);
    ("one_two", [2; 1], one_two)
  ]

let test_map =
  [
    "empty" >:: ([] === elements (map succ empty));
    "one_two" >:: ([2; 3] === elements (map succ one_two));
  ]

let test_iter =
  let add_to acc elt =
    acc := elt :: !acc in

  let test output input =
    let acc = ref [] in
    iter (add_to acc) input;
    assert_equal ~printer:(pp_list pp_int) output !acc in

  map_test test [
    ("empty", [], empty);
    ("one_two", [2; 1], one_two)
  ]

let test_memoize =
  let test output input =
    let memoized = memoize input in
    assert_equal ~printer:(pp_list pp_int) output (elements memoized) in

  map_test test [
    ("empty", [], empty);
    ("one_two", [1; 2], one_two)
  ]

module IntSet = Set.Make(struct type t = int let compare = compare end)

let test_of_set =
  let test output input =
    let set = IntSet.of_list input in
    let enum = of_set (module IntSet) set in
    assert_equal ~printer:(pp_list pp_int) output (elements enum) in

  map_test test [
    ("empty", [], []);
    ("one_two", [1; 2], [1; 2]);
  ]

let test_constant = [
  "one" >:: ([1] === elements (constant 1));
]

let test_constant_delayed = [
  "one" >:: ([1] === elements (constant_delayed (fun () -> 1)));
]

let test_range = [
  "empty" >::
  ([] === elements (range 10 0));
  "non-empty" >::
  ([12; 13; 14; 15] === elements (range 12 15));
]

let test_filter = [
  "empty" >::
  ([] === elements (filter (fun x -> x mod 2 = 0) empty));
  "empty" >::
  ([] === elements (filter (fun x -> x mod 2 = 0) (range 1 1)));
  "non-empty" >::
  ([0; 2] === elements (filter (fun x -> x mod 2 = 0) (range 0 2)));
]

let test_partition =
  let is_pair n =
    (n mod 2) == 0 in

  let test output input =
    let (p1, p2) = partition is_pair input in
    let printer = pp_pair (pp_list pp_int) (pp_list pp_int) in
    assert_equal ~printer output (elements p1, elements p2) in

  map_test test [
    ("empty", ([], []), empty);
    ("balanced", ([2], [1]), one_two);
    ("ony_true", ([2; 4], []), make [2; 4]);
    ("ony_false", ([], [1; 3]), make [1; 3]);
    ("unbalanced", ([4; 6], [1; 3; 5]), make [1; 3; 4; 5; 6]);
  ]

let test_append =
  let test output (input_1, input_2) =
    let appended = append input_1 input_2 in
    assert_equal ~printer:(pp_list pp_int) output (elements appended) in

  map_test test [
    ("empty_empty", [], (empty, empty));
    ("empty_one_two", [1; 2], (empty, one_two));
    ("one_two_empty", [1; 2], (one_two, empty));
    ("one_two_three", [1; 2; 3], (one_two, make [3]));
  ]

let test_interleave =
  let test output (input_1, input_2) =
    let interleaved = interleave input_1 input_2 in
    assert_equal ~printer:(pp_list pp_int) output (elements interleaved) in

  map_test test [
    ("empty_empty", [], (empty, empty));
    ("empty_one_two", [1; 2], (empty, one_two));
    ("one_two_empty", [1; 2], (one_two, empty));
    ("one_two_three", [1; 3; 2], (one_two, make [3]));
    ("three_one_two", [3; 1; 2], (make [3], one_two));
    ("one_two_three_four", [1; 3; 2; 4], (make [1; 2], make [3; 4]));
  ]

let test_product =
  let test output (input_1, input_2) =
    let printer = pp_list (pp_pair pp_int pp_int) in
    let combined = product input_1 input_2 in
    assert_equal ~printer output (elements combined) in

  map_test test [
    ("empty_empty", [], (empty, empty));
    ("empty_one_two", [], (empty, one_two));
    ("one_two_empty", [], (one_two, empty));
    ("distribute_left", [(1, 3); (2, 3)], (one_two, make [3]));
    ("distribute_right", [(3, 1); (3, 2)], (make [3], one_two));
    ("distribute_both", [(1, 3); (2, 3); (1, 4); (2, 4)], (one_two, make [3; 4]));
  ]

let test_bitset =
  let test output input =
    let (n, k) = input in
    let set = bitset ?k n in
    assert_equal ~printer:(pp_list pp_int) output (elements set) in

  map_test test [
    ("n_zero", [0b0], (0, None));
    ("n_one", [0b0; 0b1], (1, None));
    ("n_two", [0b00; 0b01; 0b10; 0b11], (2, None));
    ("n_three", [0b000; 0b001; 0b010; 0b100; 0b011; 0b101; 0b110; 0b111], (3, None));
    ("n_two_k_zero", [0b0], (2, Some 0));
    ("n_two_k_one", [0b00; 0b01; 0b10], (2, Some 1));
    ("n_two_k_two", [0b00; 0b01; 0b10; 0b11], (2, Some 2));
    ("n_three_k_two", [0b000; 0b001; 0b010; 0b100; 0b011; 0b101; 0b110], (3, Some 2));
  ]

let test_subset =
  let test output (enum, k) =
    let subset_enum_list = elements (subset ?k enum) in
    let subset_list_list = List.map elements subset_enum_list in
    let printer = pp_list (pp_list (pp_list pp_int)) in
    assert_equal ~printer output subset_list_list in

  map_test test [
    ("empty", [], ([], None));
    ("singleton_empty", [
        [[]];  (* empty subset *)
        [];    (* 1-tuples from [] *)
      ], ([empty], None));
    ("singleton_one_two", [
        [[]];       (* empty subset *)
        [[1]; [2]]  (* 1-tuples from [1; 2] *)
      ], ([one_two], None));
    ("pair_empty_one_two", [
        [[]];        (* empty subset *)
        [];          (* 1-tuples from [] *)
        [[1]; [2]];  (* 1-tuples from [1; 2] *)
        [];          (* 2-tuples from [] and [1; 2] *)
      ], ([empty; one_two], None));
    ("pair_one_two_empty", [
        [[]];
        [];
        [[1]; [2]];
        [];
      ], ([empty; one_two], None));
    ("pair_one_two_three_four", [
        [[]];
        [[1]; [2]];
        [[3]; [4]];
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]]
      ], ([one_two; make [3; 4]], None));
    ("triple", [
        [[]];                                          (* empty subset *)
        [[1]; [2]];                                    (* 1-tuples from [1; 2] *)
        [[3]; [4]];                                    (* 1-tuples from [3; 4] *)
        [[5]];                                         (* 1-tuples from [5] *)
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]];              (* 2-tuples from [1; 2] and [3; 4] *)
        [[1; 5]; [2; 5]];                              (* 2-tuples from [1; 2] and [5] *)
        [[3; 5]; [4; 5]];                              (* 2-tuples from [3; 4] and [5] *)
        [[1; 3; 5]; [2; 3; 5]; [1; 4; 5]; [2; 4; 5]];  (* 3-tuples from [1; 2], [3; 4] and [5] *)
      ], ([one_two; make [3; 4]; make [5]], None));
    ("singleton_empty_k_zero", [
        [[]];
      ], ([empty], Some 0));
    ("singleton_one_two_k_zero", [
        [[]];
      ], ([one_two], Some 0));
    ("pair_empty_one_two_k_zero", [
        [[]];
      ], ([empty; one_two], Some 0));
    ("pair_one_two_three_four_k_zero", [
        [[]];
      ], ([one_two; make [3; 4]], Some 0));
    ("empty_k_one", [
        [[]];
        [];
      ], ([empty], Some 1));
    ("empty_k_one", [
        [[]];
        [[1]; [2]];
      ], ([one_two], Some 1));
    ("pair_empty_one_two_k_one", [
        [[]];
        [];
        [[1]; [2]];
      ], ([empty; one_two], Some 1));
    ("pair_one_two_three_four_k_one", [
        [[]];
        [[1]; [2]];
        [[3]; [4]];
      ], ([one_two; make [3; 4]], Some 1));
    ("pair_one_two_three_four_k_two", [
        [[]];
        [[1]; [2]];
        [[3]; [4]];
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]]
      ], ([one_two; make [3; 4]], Some 2));
    ("pair_one_two_three_four_k_three", [
        [[]];
        [[1]; [2]];
        [[3]; [4]];
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]]
      ], ([one_two; make [3; 4]], Some 3));
  ]

let test_squash =
  let test output input =
    let squashed = squash input in
    assert_equal ~printer:(pp_list pp_int) output (elements squashed) in

  map_test test [
    ("empty", [], make []);
    ("one_two", [1; 2], make [one_two]);
    ("empty_one_two", [1; 2], make [empty; one_two]);
    ("one_two_empty", [1; 2], make [one_two; empty]);
    ("two_enumerators", [1; 2; 3; 4], make [one_two; make [3; 4]]);
    ("three_enumerators", [1; 2; 3; 4], make [one_two; make [3]; make [4]]);
  ]

let test_round_robin =
  let test output input =
    let robin = round_robin input in
    assert_equal ~printer:(pp_list pp_int) output (elements robin) in

  map_test test [
    ("empty", [], make []);
    ("one_two", [1; 2], make [one_two]);
    ("empty_one_two", [1; 2], make [empty; one_two]);
    ("one_two_empty", [1; 2], make [one_two; empty]);
    ("two_enumerators", [1; 3; 2; 4], make [one_two; make [3; 4]]);
    ("three_enumerators", [1; 3; 4; 2], make [one_two; make [3]; make [4]]);
  ]

let test_firstn _ =
  for a = 0 to 4 do
    for b = 0 to 8 do
      for c = 0 to 9 do
        let e = range a b in
        let n = List.length (elements e) in
        let (l, e') = firstn (Int64.of_int c) e in
        assert_equal n (max ((b - a) + 1) 0);
        assert_equal (List.length l)  (min c n);
        assert_equal (List.length (elements e'))  (n - (min c n))
      done
    done
  done

let test_choose_k_from_list =
  let test output (enum, k) =
    let chosen_enum_list = elements (choose_k_from_list ~k enum) in
    let chosen_list_list = List.map elements chosen_enum_list in
    let printer = pp_list (pp_list (pp_list pp_int)) in
    assert_equal ~printer output chosen_list_list in

  map_test test [
    ("singleton_empty_k_one", [
        [];
      ], ([empty], 1));
    ("singleton_one_two_k_one", [
        [[1]; [2]];
      ], ([one_two], 1));
    ("pair_one_two_three_four_k_one", [
        [[1]; [2]];  (* singletons from [1; 2] *)
        [[3]; [4]];  (* singletons from [3; 4] *)
      ], ([one_two; make [3; 4]], 1));
    ("triple_k_one", [
        [[1]; [2]];
        [[3]; [4]];
        [[5]];
      ], ([one_two; make [3; 4]; make [5]], 1));
    ("singleton_empty_k_two", [
        [];
      ], ([empty], 2));
    ("singleton_one_two_k_two", [
        [[1]; [2]];
      ], ([one_two], 2));
    ("pair_one_two_three_four_k_two", [
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]];
      ], ([one_two; make [3; 4]], 2));
    ("triple_k_two", [
        [[1; 3]; [2; 3]; [1; 4]; [2; 4]];  (* pairs from [1; 2] and [3; 4] *)
        [[1; 5]; [2; 5]];                  (* pairs from [1; 2] and [5] *)
        [[3; 5]; [4; 5]];                  (* pairs from [3; 4] and [5] *)
      ], ([one_two; make [3; 4]; make [5]], 2));
  ] @ [
    "k_zero" >:: fun _ ->
      assert_raises
        (Invalid_argument "choose_k_from_list: k must be greater than zero")
        (fun () -> choose_k_from_list ~k:0 [empty]);
  ]

let test_depth =
  let test output input =
    assert_equal ~printer:pp_int output (depth input) in

  map_test test [
    ("empty", 0, empty);
    ("map", 1, map succ one_two);
  ]

let suite = "enumerator" >::: [
    "elements_make" >::: test_elements_make;
    "nth" >::: test_nth;
    "size" >::: test_size;
    "is_empty" >::: test_is_empty;
    "fold_left" >::: test_fold_left;
    "map" >::: test_map;
    "iter" >::: test_iter;
    "memoize" >::: test_memoize;
    "of_set" >::: test_of_set;
    "constant" >::: test_constant;
    "constant_delayed" >::: test_constant_delayed;
    "range" >::: test_range;
    "firstn" >:: test_firstn;
    "filter" >::: test_filter;
    "partition" >::: test_partition;
    "bitset" >::: test_bitset;
    "append" >::: test_append;
    "interleave" >::: test_interleave;
    "product" >::: test_product;
    "subset" >::: test_subset;
    "squash" >::: test_squash;
    "round_robin" >::: test_round_robin;
    "choose_k_from_list" >::: test_choose_k_from_list;
    "depth" >::: test_depth;
  ]

let () = run_test_tt_main suite
