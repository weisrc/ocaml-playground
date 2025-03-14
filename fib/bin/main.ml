open Printf
open Fib.BigInt

let time_it f =
  let start = Sys.time () in
  let out = f () in
  (Sys.time () -. start, out)

let rec bad_fib n =
  match n with
  | 0 -> [ 0 ]
  | 1 -> [ 1 ]
  | _ -> bad_fib (n - 1) +! bad_fib (n - 2)

let lin_fib (n : int) =
  let rec inner (i : int) (a : bigint) (b : bigint) =
    if i = n then a else inner (i + 1) b (a +! b)
  in
  inner 0 [ 0 ] [ 1 ]

type mat2x2 = bigint * bigint * bigint * bigint

let ( @ ) ((a00, a01, a10, a11) : mat2x2) ((b00, b01, b10, b11) : mat2x2) :
    mat2x2 =
  ( (a00 *! b00) +! (a01 *! b10),
    (a00 *! b01) +! (a01 *! b11),
    (a10 *! b00) +! (a11 *! b10),
    (a10 *! b01) +! (a11 *! b11) )

let is_even n = n mod 2 = 0

let rec ( ^ ) (mat : mat2x2) n : mat2x2 =
  if n = 1 then mat
  else if is_even n then
    let res_mat = mat ^ (n / 2) in
    res_mat @ res_mat
  else
    let res_mat = mat ^ (n / 2) in
    mat @ res_mat @ res_mat

let second_of_mat ((_, a01, _, _) : mat2x2) = a01
let log_fib n = second_of_mat (([ 1 ], [ 1 ], [ 1 ], [ 0 ]) ^ n)
let n = 500_000
let log_time, log_out = time_it (fun () -> log_fib n);;

printf "log_time: %f\n%!" log_time

let lin_time, lin_out = time_it (fun () -> lin_fib n);;

printf "lin_time: %f\n%!" lin_time;;

(* let bad_time, bad_out = time_it (fun () -> bad_fib n);;

printf "bad_time: %f\n%!" bad_time;; *)

printf "same: %s\n%!" (if log_out = lin_out then "true" else "false");;
printf "digit count: %d\n%!" (string_of_bigint lin_out |> String.length)