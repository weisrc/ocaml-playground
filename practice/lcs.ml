open Printf
open Array

let lcs (a : string) (b : string) =
  let a_len = String.length a in
  let b_len = String.length b in
  let table = make_matrix a_len b_len 0 in

  let a_folder (a_i, b_char, b_i, max_i, max_len) a_char =
    let len =
      if a_char = b_char then
        if a_i = 0 || b_i = 0 then 1 else table.(a_i - 1).(b_i - 1) + 1
      else 0
    in
    table.(a_i).(b_i) <- len;
    if len > max_len then (a_i + 1, b_char, b_i, a_i, len)
    else (a_i + 1, b_char, b_i, max_i, max_len)
  in

  let b_folder (b_i, max_i, max_len) b_char =
    let _, _, b_i, max_i, max_len =
      String.fold_left a_folder (0, b_char, b_i, max_i, max_len) a
    in
    (b_i + 1, max_i, max_len)
  in

  let _, max_i, max_len = String.fold_left b_folder (0, 0, 0) b in
  String.sub a (max_i - max_len + 1) max_len
;;

Printf.printf "[%s]\n" (lcs "hello !world" "hello !there")