open Printf

let rec rec_pow exp base div =
  if exp = 0 then 1 else base * rec_pow (exp - 1) base div mod div

let tail_pow exp base div =
  let rec helper exp acc =
    if exp = 0 then acc else helper (exp - 1) (base * acc mod div)
  in
  helper exp 1

let cps_pow exp base div =
  let rec helper exp k =
    if exp = 0 then k 1 else helper (exp - 1) (fun x -> k (base * x mod div))
  in
  helper exp (fun x -> x)

let rec gen_pow exp =
  if exp = 0 then fun _ _ -> 1
  else
    let gen = gen_pow (exp - 1) in
    fun base div -> base * gen base div mod div

let exp = 12345678
let base = 2
let div = 1234567
let ans = 1177675

let time_it f =
  let start = Sys.time () in
  let out = f () in
  (Sys.time () -. start, out)

let rec_time, rec_out = time_it (fun () -> rec_pow exp base div)
let tail_time, tail_out = time_it (fun () -> tail_pow exp base div)
let cps_time, cps_out = time_it (fun () -> cps_pow exp base div)
let gen_prep_time, generated_pow = time_it (fun () -> gen_pow exp)
let gen_time, gen_out = time_it (fun () -> generated_pow base div);;

assert (rec_out = ans);
assert (tail_out = ans);
assert (cps_out = ans);
assert (gen_out = ans);

Printf.printf "rec: %f \ntail: %f \ncps: %f \ngen_prep: %f \ngen: %f \n"
  rec_time tail_time cps_time gen_prep_time gen_time
