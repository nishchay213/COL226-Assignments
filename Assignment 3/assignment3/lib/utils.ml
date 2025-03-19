let string_to_vector s =
  let s = String.sub s 1 (String.length s - 2) in
  let s = String.split_on_char ',' s in
  let list_vector = List.map float_of_string s in
  Array.of_list list_vector