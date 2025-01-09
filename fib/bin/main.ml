open Printf

let time_it f =
  let start = Sys.time () in
  let out = f () in
  (Sys.time () -. start, out)

let rec bad_fib n =
  match n with 0 -> 0 | 1 -> 1 | _ -> bad_fib (n - 1) + bad_fib (n - 2)

let lin_fib n =
  let rec inner i a b = if i = n then a else inner (i + 1) b (a + b) in
  inner 0 0 1

type mat2x2 = int * int * int * int

let ( * ) ((a00, a01, a10, a11) : mat2x2) ((b00, b01, b10, b11) : mat2x2) :
    mat2x2 =
  ( (a00 * b00) + (a01 * b10),
    (a00 * b01) + (a01 * b11),
    (a10 * b00) + (a11 * b10),
    (a10 * b01) + (a11 * b11) )

let is_even n = n mod 2 = 0

let rec ( ^ ) (mat : mat2x2) n : mat2x2 =
  if n = 1 then mat
  else if is_even n then
    let res_mat = mat ^ (n / 2) in
    res_mat * res_mat
  else
    let res_mat = mat ^ (n / 2) in
    mat * res_mat * res_mat

let _string_of_mat ((a00, a01, a10, a11) : mat2x2) : string =
  sprintf "[%d,\t%d\n %d, \t%d]" a00 a01 a10 a11

let second_of_mat ((_, a01, _, _) : mat2x2) : int = a01
let log_fib n = second_of_mat ((1, 1, 1, 0) ^ n)
let n = 10000000;;

let (t, o) = time_it (fun () -> log_fib n) in printf "log_fib: %d, %f\n" o t;;
let (t, o) = time_it (fun () -> lin_fib n) in printf "lin_fib: %d, %f\n" o t;;
let (t, o) = time_it (fun () -> bad_fib 9) in printf "bad_fib: %d, %f\n" o t;;
