type 'a suspense = Suspense of (unit -> 'a)

let force (Suspense s) = s ()

type 'a stream = { head : 'a; tail : 'a stream suspense }

let rec take (n : int) (s : 'a stream) : 'a list =
  if n <= 0 then [] else [ s.head ] @ take (n - 1) (force s.tail)

let rec power_series i =
  {
    head = 1.0 /. (2. ** float_of_int i);
    tail = Suspense (fun () -> power_series (i + 1));
  }

let rec print_repeated f l =
  match l with
  | [] -> ()
  | x :: xs ->
      f x;
      print_repeated f xs

let print_list f l =
  print_string "[ ";
  print_repeated
    (fun x ->
      f x;
      print_char ' ')
    l;
  print_char ']';
  print_newline ()

let print_option f option =
  match option with
  | None -> print_string "None "
  | Some value ->
      print_string "Some ";
      f value

let rec cycle list =
  let rec helper current =
    match current with
    | [] -> helper list
    | x :: xs -> { head = x; tail = Suspense (fun () -> helper xs) }
  in
  helper list

let rec triple_fib (a : int) (b : int) (c : int) : int stream =
  { head = a; tail = Suspense (fun () -> triple_fib b c (a + b + c)) }

let rec merge s1 s2 =
  if s1.head < s2.head then
    { head = s1.head; tail = Suspense (fun () -> merge (force s1.tail) s2) }
  else if s1.head > s2.head then
    { head = s2.head; tail = Suspense (fun () -> merge s1 (force s2.tail)) }
  else
    let tail = Suspense (fun () -> merge (force s1.tail) (force s2.tail)) in
    { head = s1.head; tail }

let rec empty_list = { head = None; tail = Suspense (fun () -> empty_list) }

let change_gen all_coins total =
  let rec helper coins remaining (acc : int list)
      (next : unit -> int list option stream) : int list option stream =
    if remaining < 0 then next ()
    else
      match (coins, remaining) with
      | _, 0 -> { head = Some acc; tail = Suspense next }
      | [], _ -> next ()
      | c :: cs, _ ->
          helper coins (remaining - c) (c :: acc) (fun () ->
              helper cs remaining acc next)
  in
  helper all_coins total [] (fun () -> empty_list)
;;

(* print_list print_int (take 10 (cycle [ 1; 2; 3 ])) *)

(* print_list print_int (take 10 (triple_fib 0 1 1)) *)

(* print_list print_int (take 20 (merge (cycle [ 1; 3; 5 ]) (cycle [ 0; 2; 4 ]))) *)

print_repeated
  (print_option (print_list print_int))
  (take 1000 (change_gen [ 100; 50; 20; 10; 2; 1 ] 1000))
