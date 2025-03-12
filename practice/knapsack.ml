open Array

let knapsack (ids : int array) (values : int array) (sizes : int array)
    (capacity : int) =
  assert (length sizes = length values);
  let table = make_matrix (length sizes + 1) (capacity + 1) None in
  let rec helper item remaining =
    if item = 0 || remaining = 0 then (0, 0)
    else
      match table.(item).(remaining) with
      | Some total -> total
      | None ->
          let id = ids.(item - 1) in
          let size = sizes.(item - 1) in
          let value = values.(item - 1) in
          let skip = helper (item - 1) remaining in
          let out =
            if size > remaining then skip
            else
              let add = helper (item - 1) (remaining - size) in
              let add_value = snd add + value in
              if add_value > snd skip then (fst add + id, add_value) else skip
          in
          set table.(item) remaining (Some out);
          out
  in
  fst (helper (length values) capacity)

let ids = [| 1; 20; 300; 4000; 50000 |]
let values = [| 1; 2; 3; 4; 5 |]
let sizes = [| 1; 2; 3; 4; 5 |];;

print_int (knapsack ids values sizes 9);;
print_newline ()
