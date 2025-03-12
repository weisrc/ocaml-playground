let rec rec_pow exp base = if exp = 0 then 1 else base * rec_pow (exp - 1) base

let tail_pow exp base =
  let rec helper exp acc =
    if exp = 0 then acc else helper (exp - 1) (base * acc)
  in
  helper exp 1

let cps_pow exp base =
  let rec helper exp k =
    if exp = 0 then k 1 else helper (exp - 1) (fun x -> k base * x)
  in
  helper exp (fun x -> x)

let rec gen_pow exp =
  if exp = 0 then fun _ -> 1
  else
    let gen = gen_pow (exp - 1) in
    fun base -> base * gen base

let exp = 60
let base = 2
let ans = 1152921504606846976;;

assert (rec_pow exp base = ans);
assert (tail_pow exp base = ans);
assert (cps_pow exp base = ans)
