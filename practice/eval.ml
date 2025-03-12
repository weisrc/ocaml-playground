let is_alpha c =
  match c with 'a' .. 'z' -> true | 'A' .. 'Z' -> true | _ -> false

let is_digit c = match c with '0' .. '9' -> true | _ -> false
let is_space c = c = ' ' || c = '\t' || c = '\n'
let is_name c = is_alpha c || is_digit c || c = '_'

type 'a consume_t = Consumed of (string * string) | Failed

let consume_char text =
  let text_len = String.length text in
  if text_len > 0 then
    Consumed (String.sub text 0 1, String.sub text 1 (text_len - 1))
  else Failed

let consume_token (token : string) =
  let token_len = String.length token in
  let starts_with_token = String.starts_with ~prefix:token in
  fun text ->
    let sub_len = String.length text - token_len in
    if starts_with_token text then
      Consumed (token, String.sub text token_len sub_len)
    else Failed

let consume_char_with f text =
  match consume_char text with
  | Failed -> Failed
  | Consumed (c, text) ->
      if f (String.get c 0) then Consumed (c, text) else Failed

let rec consume_repeat consumer text =
  match consumer text with
  | Failed -> Failed
  | Consumed (part, rest) -> (
      match consume_repeat consumer rest with
      | Consumed (sub_part, sub_rest) -> Consumed (part ^ sub_part, sub_rest)
      | Failed -> Consumed (part, rest))

let consume_alpha = consume_char_with is_alpha
let consume_digit = consume_char_with is_digit
let consume_space = consume_char_with is_space
let consume_many_digits = consume_repeat consume_digit
let consume_many_spaces = consume_repeat consume_space

let consume_symbol text =
  match consume_alpha text with
  | Failed -> Failed
  | Consumed (first, rest) -> consume_repeat (consume_char_with is_name) text

type literal = String of string | Int of int | Float of float | Bool of bool

type expression =
  | Let of { symbol : string; expression : expression }
  | Literal of { value : literal }
  | Call of { symbol : string; argument : expression }
