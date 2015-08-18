open OUnit2
open Enumerator

let pp_int = string_of_int

let pp_list pp_elem l =
  "[" ^ String.concat "; " (List.map pp_elem l) ^ "]"

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

let test_append = [
  "empty" >::
  ([] === elements (append empty empty));
  "nonempty" >::
  ([0; 1] === elements (append (range 0 1) empty));
  "nonempty" >::
  ([0; 1] === elements (append empty (range 0 1)));
  "nonempty" >::
  ([0; 1; 2; 3] === elements (append (range 0 1) (range 2 3)));
]

let test_product =
  let (===) a b = fun _ ->
    assert_equal a b
  in
  [
    "empty" >::
    ([] === elements (product empty empty));
    "empty" >::
    ([] === elements (product empty (constant 1)));
    "empty" >::
    ([] === elements (product (constant 1) empty));
    "singleton" >::
    ([1, 1] === elements (product (constant 1) (constant 1)));
    "lexicographic" >::
    ([0, 2; 1, 2; 0, 3; 1, 3] === elements (product (range 0 1) (range 2 3)));
  ]


let test_bitset =
  let (===) a b = fun _ ->
    assert_equal a b in
  [
    "size 2">::
    ([0b00; 0b01; 0b10; 0b11] === (elements (bitset 2)));

    "size 3">::
    ([0b000; 0b001; 0b010; 0b100; 0b011; 0b101; 0b110; 0b111 ] === (elements (bitset 3)));

    "cardinal">::
    (fun _ -> assert_equal
        (List.length (elements (bitset 4)))
        (1 lsl 4))
  ]

let test_subset =
  let (===) a (b: 'a list Enumerator.t Enumerator.t) = fun _ ->
    let b : 'a list list =
      List.fold_right
        (fun e acc -> Enumerator.elements e @ acc)
        (elements b)
        []
    in
    assert_equal a b in
  [
    "empty" >::
    ([[]] === (subset [empty]));

    "singleton" >::
    ([[]; [1]; [2]; [3]] === (subset [range 1 3]));

    "pair" >::
    (
      [
        [];
        [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ];
        [ 1; 4 ]; [ 2; 4 ]; [ 3; 4 ]; [ 1; 5 ]; [ 2; 5 ]; [ 3; 5 ]
      ]
      ===
      (subset [range 1 3; range 4 5]));
    "triple" >::
    (
      [
        [];
        [1]; [2]; [3]; [4]; [5]; [6];
        [1; 4]; [2; 4]; [3; 4]; [1; 5]; [2; 5]; [3; 5]; [1; 6]; [2; 6]; [3; 6]; [4; 6]; [5; 6];
        [1; 4; 6]; [2; 4; 6]; [3; 4; 6]; [1; 5; 6]; [2; 5; 6]; [3; 5; 6]
      ]
      ===
      (subset [range 1 3; range 4 5; range 6 6]));

  ]

let test_squash = [
  begin
    "empty" >::
    ([] === elements (squash (constant empty)))
  end;

  begin
    "singleton" >::
    ([1] === elements (squash (constant (constant 1))))
  end;

  begin
    "range" >::
    ([1; 2] === elements (squash (constant (range 1 2))))
  end;

  begin
    let c = constant in
    let (--) = range in
    let (@@) = append in
    let enum =
      (c (1--2))@@(c (3--4)@@(c (c 5)))
    in
    "range"
    >::
    ([1; 2; 3; 4; 5] === elements (squash enum))
  end;
]


let test_round_robin = [
  begin
    "empty" >::
    ([] === elements (round_robin (constant empty)))
  end;

  begin
    "singleton" >::
    ([1] === elements (round_robin (constant (constant 1))))
  end;

  begin
    "range" >::
    ([1; 2] === elements (round_robin (constant (range 1 2))))
  end;

  begin
    let c = constant in
    let (--) = range in
    let (@@) = append in
    let enum =
      (c (1--2))@@(c (3--4)@@(c (c 5)))
    in
    "range" >::
    ([1; 3; 5; 2; 4] === elements (round_robin enum))
  end;
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

let test_choose_k_from_list = [
  begin
    "empty" >:: (elements (make []) === elements (choose_k_from_list ~k:1 []))
  end;
  begin
    let enums = [make []; make []] in
    "list_of_empty" >:: ([] === elements (squash (choose_k_from_list ~k:1 enums)))
  end;
  begin
    let enums = [make [1; 2; 3]; make [4; 5]] in
    let combinations = elements (squash (choose_k_from_list ~k:2 enums)) in
    "two_lists" >:: fun _ ->
      assert_equal ~printer:(pp_list (pp_list pp_int))
        [[1; 4]; [2; 4]; [3; 4]; [1; 5]; [2; 5]; [3; 5]]
        combinations
  end
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
    "constant" >::: test_constant;
    "constant_delayed" >::: test_constant_delayed;
    "range" >::: test_range;
    "filter" >::: test_filter;
    "product" >::: test_product;
    "append" >::: test_append;
    "bitset" >::: test_bitset;
    "subset" >::: test_subset;
    "squash" >::: test_squash;
    "round_robin" >::: test_round_robin;
    "firstn" >:: test_firstn;
    "choose_k_from_list" >::: test_choose_k_from_list;
  ]

let () = run_test_tt_main suite
