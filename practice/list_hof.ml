let rec map f l = match l with [] -> [] | item :: rest -> f item :: map f rest

let rec map_tr f l acc =
  match l with [] -> acc | item :: rest -> map_tr f rest (acc @ [ f item ])

let rec map_cps f l return =
  match l with
  | [] -> return []
  | item :: rest -> map_cps f rest (fun x -> return (f item :: x))

let rec filter_cps f l return =
  match l with
  | [] -> return []
  | item :: rest ->
      filter_cps f rest (fun x -> return (if f item then item :: x else x))

let rec fold_left f l acc =
  match l with [] -> acc | x :: xs -> fold_left f xs (f x acc)

let fold_right f l acc =
  let rec helper l acc return =
    match l with
    | [] -> return acc
    | item :: rest -> helper rest acc (fun x -> return (f item x))
  in
  helper l acc (fun x -> x)

let fold_right f l acc =
  match l with [] -> acc | x :: xs -> f x (fold_right f xs acc)

let f x = x + 1
let id x = x
let is_odd x = x mod 2 = 1
let arr = [ 1; 2; 3; 4; 5; 6 ]
let out = map f arr
let out_tr = map_tr f arr []
let out_cps = map_cps f arr id
let filtered_cps = filter_cps is_odd arr id
let folded_left = fold_left (fun x y -> x :: y) arr []
let folded_right = fold_right (fun x y -> x :: y) arr []
