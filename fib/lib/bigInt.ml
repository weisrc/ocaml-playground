type bigint = int list

let base = 1000000000000000000

let rec add (list_a : bigint) (list_b : bigint) (carry : int) =
  let inner sum rest_a rest_b =
    let carry = sum / base in
    let value = sum mod base in
    value :: add rest_a rest_b carry
  in
  match (list_a, list_b) with
  | [], [] -> if carry = 0 then [] else [ carry ]
  | a :: rest_a, [] -> inner (a + carry) rest_a []
  | [], b :: rest_b -> inner (b + carry) [] rest_b
  | a :: rest_a, b :: rest_b -> inner (a + b + carry) rest_a rest_b

let ( +! ) (a : bigint) (b : bigint) = add a b 0
let ( *! ) (a : bigint) (b : bigint) = add a b 0

let pad_string s length =
  let remaining = length - String.length s in
  if remaining <= 0 then s else String.make remaining '0' ^ s

let rec string_of_bigint (v : bigint) =
  match v with
  | [] -> ""
  | [ single ] -> string_of_int single
  | first :: rest -> string_of_bigint rest ^ pad_string (string_of_int first) 18
