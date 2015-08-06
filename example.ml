let print_target (ip, port) =
  Printf.printf "[%s]:%d\n" ip port

let () =
  let addresses = Enumerator.make ["2001:db8::1"; "2001:db8::2"] in
  let ports = Enumerator.range 1 1024 in
  let targets = Enumerator.product addresses ports in
  Enumerator.iter print_target targets
