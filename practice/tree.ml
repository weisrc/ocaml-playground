type 't tree = Tree of ('t * 't tree * 't tree) | Empty

let rec map f tree =
  match tree with
  | Empty -> Empty
  | Tree (item, left, right) -> Tree (f item, map f left, map f right)

let rec filter f tree =
  match tree with
  | Empty -> []
  | Tree (item, left, right) ->
      let filtered = filter f left @ filter f right in
      if f item then item :: filtered else filtered

let test = Tree (6, Empty, Tree (7, Empty, Empty))
let f x = x + 1
let is_odd x = x mod 2 = 1
let mapped = map f test
let filtered = filter is_odd test
