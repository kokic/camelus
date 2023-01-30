
let parse parser s = match parser s with 
  Some (res, _) -> Some res | None -> None

let return x input = Some (x, input)

let token predicate = fun s -> if predicate s.[0] 
  then return s.[0] (String.sub s 1 (String.length s - 1)) 
  else None
let exactly x = token ((==) x)

let exactly' x = let n = String.length x in 
  fun s -> if x == String.sub s 0 n 
  then return x (String.sub s n (String.length
   s - 1))
  else None



let var = exactly' "var"

let lbrace = exactly '{'

let tuple_map f g x = (f (fst x), g (snd x))
let tuple_map' f x = tuple_map f Fun.id x
let print_tuple x = print_endline ("(\"" ^ fst x ^ "\", \"" ^ snd x ^ "\")")

let empty = ("_", "_")
let case o f = match o with Some x -> f x | None -> empty
let case' o = match o with Some x -> x | None -> empty 

;; 
let tuple = case' (var "var x = 1") in
  print_tuple tuple


