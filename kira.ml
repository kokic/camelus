
let parse parser s = match parser s with 
  Some (res, _) -> Some res | None -> None

let return x input = Some (x, input)

let exactly x s = if x == s.[0] 
  then return x (String.sub s 1 (String.length s - 1)) 
  else None

let lbrace = exactly '{'

let tuple_map t f g = (f (fst t), g (snd t))
let tuple_map' t f = tuple_map t f Fun.id
let print_tuple t = print_endline ("(\"" ^ fst t ^ "\", \"" ^ snd t ^ "\")")

let empty = ("_", "_")


;; 
let tuple = match lbrace "{x}" with 
  Some x -> tuple_map' x Char.escaped | None -> empty in
  print_tuple tuple


