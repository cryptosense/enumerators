#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "enumerators" @@ fun config ->
  Ok [ Pkg.mllib "src/enumerators.mllib"
     ; Pkg.test "test/test_suite"
     ]
