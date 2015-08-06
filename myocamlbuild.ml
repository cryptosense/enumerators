open Ocamlbuild_plugin

let bits =
  let b = Sys.word_size in
  match b with
  | 32 | 64 -> b
  | _ -> failwith (Printf.sprintf "Unsupported word size: %d\n" b)

let additional_dirs = [
  Printf.sprintf "%d" bits;
]

let after_rules () =
  Options.include_dirs := !Options.include_dirs @ additional_dirs

let () =
  dispatch begin function
    | After_rules -> after_rules ()
    | _ -> ()
  end
