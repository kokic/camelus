
let parse parser s = match parser s with 
  Some (res, _) -> Some res | None -> None

let return x input = Some (x, input)

let token predicate = fun s -> if predicate s.[0] 
  then return s.[0] (String.sub s 1 (String.length s - 1)) 
  else None

let exactly x = token ((==) x)

let lbrace = exactly '{'

let tuple_map f g x = (f (fst x), g (snd x))
let tuple_map' f x = tuple_map f Fun.id x
let print_tuple x = print_endline ("(\"" ^ fst x ^ "\", \"" ^ snd x ^ "\")")

let empty = ("_", "_")
let case o f = match o with Some x -> f x | None -> empty

;; 
let tuple = case (lbrace "{x}") (tuple_map' Char.escaped) in
  print_tuple tuple


