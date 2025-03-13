exception Impossible

let exn_change coins target =
  let rec helper coins target acc =
    if target = 0 then acc
    else
      match coins with
      | [] -> raise Impossible
      | c :: cs -> (
          if c > target then helper cs target acc
          else
            try helper coins (target - c) (c :: acc)
            with Impossible -> helper cs target acc)
  in
  try helper coins target [] with Impossible -> []

let raw_change coins target =
  let rec helper coins target acc =
    if target = 0 then acc
    else
      match coins with
      | [] -> []
      | c :: cs -> (
          if c > target then helper cs target acc
          else
            match helper coins (target - c) (c :: acc) with
            | [] -> helper cs target acc
            | out -> out)
  in
  helper coins target []

let opt_change coins target =
  let rec helper coins target acc =
    if target = 0 then Some acc
    else
      match coins with
      | [] -> None
      | c :: cs -> (
          if c > target then helper cs target acc
          else
            match helper coins (target - c) (c :: acc) with
            | Some out -> Some out
            | None -> helper cs target acc)
  in
  match helper coins target [] with Some out -> out | None -> []

let cps_change coins target =
  let rec helper coins target acc fail =
    if target = 0 then acc
    else
      match coins with
      | [] -> fail ()
      | c :: cs ->
          if c > target then helper cs target acc fail
          else
            helper coins (target - c) (c :: acc) (fun () ->
                helper cs target acc fail)
  in
  helper coins target [] (fun () -> [])

let coins = [ 1000; 100; 50; 20; 10; 5 ]

let time_it f =
  let start = Sys.time () in
  let out = f () in
  (Sys.time () -. start, out)

let run target =
  let exn_time, exn_out = time_it (fun () -> exn_change coins target) in
  let opt_time, opt_out = time_it (fun () -> opt_change coins target) in
  let cps_time, cps_out = time_it (fun () -> cps_change coins target) in
  let raw_time, raw_out = time_it (fun () -> raw_change coins target) in

  (* List.iter (Printf.printf "%d ") opt_out; *)
  Printf.printf " exn: %f\n opt: %f\n cps: %f\n raw: %f\n" exn_time opt_time
    cps_time raw_time;

  assert (exn_out = opt_out);
  assert (exn_out = cps_out);
  assert (exn_out = raw_out)
;;

print_string "success case:\n";
run 1234567890;
print_string "failure case:\n";
run 2501
