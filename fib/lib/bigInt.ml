type bigint = int list

let width = 9
let base = 1_000_000_000

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
let mul_by_base (n : bigint) = 0 :: n

let rec mul_by_int (n : bigint) (m : int) (carry : int) =
  let inner product rest =
    let carry = product / base in
    let value = product mod base in
    value :: mul_by_int rest m carry
  in
  match n with
  | [] -> if carry = 0 then [] else [ carry ]
  | part :: rest -> inner ((part * m) + carry) rest

let rec mul (n : bigint) (m : bigint) (acc : bigint) =
  match m with
  | [] -> acc
  | part :: rest -> mul (mul_by_base n) rest (acc +! mul_by_int n part 0)

let ( *! ) (a : bigint) (b : bigint) = mul a b [ 0 ]

let pad_string s length =
  let remaining = length - String.length s in
  if remaining <= 0 then s else String.make remaining '0' ^ s

let rec string_of_bigint (v : bigint) =
  match v with
  | [] -> ""
  | [ single ] -> string_of_int single
  | first :: rest ->
      string_of_bigint rest ^ pad_string (string_of_int first) width
