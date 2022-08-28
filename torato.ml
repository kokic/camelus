
(* basic utils *)

open List

let id x = x
let delta condition x y = if condition then x else y
let delta_lazy condition fx fy = if condition then fx () else fy ()

let last xs = nth xs (length xs - 1)
let push xs x = xs @ [x]

let rec drop n = function [] -> []
  | head :: tails as xs -> match n with 0 -> xs | _ -> (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function [] -> []
  | head :: tails -> match n with 0 -> [] | _ -> head :: take (n - 1) tails
let lizard xs = take (length xs - 1) xs

let manifold f xs trans default = match xs with [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails

let case_opt default = function None -> default | Some x -> x
let case x = case_opt (failwith "case none") x

let explode s = init (String.length s) (String.get s)
let rec repeat s = function 1 -> s | n -> s ^ repeat s (n - 1)

let string_of_pair f (x, y) = f x ^ f y
let string_of_char_pair = string_of_pair Char.escaped

let glue = fun s t -> s ^ Char.escaped t
let comma = fun trans s t -> s ^ ", " ^ trans t
let string_of_char_list xs = manifold comma xs Char.escaped ""

let unique xs = let xs' = ref [] in
  for index = 0 to length xs - 1 do
    let x = nth xs index in
    match mem x !xs' with true -> ()
      | false -> xs' := push !xs' x
  done; !xs'

let degree_at xs x = fold_left (fun u v -> u + delta (x == v) 1 0) 0 xs
let degree xs = let deg_at = degree_at xs in let xs' = unique xs in
  fold_left (fun u v -> let deg = deg_at v in delta (u > deg) u deg) 0 xs'

(* torato *)

open Printf 

let alphabets = explode "abcdefghijklmnopqrstuvwxyz" ;;


Random.self_init () ;;
let random_visit xs = nth xs (Random.int (length xs))


(* construction of adjacent *)
module AdjacentMap = Map.Make (Char)
let adjacent = ref AdjacentMap . empty ;;

let print_of_char_map key value = let s = string_of_char_list value in
  print_endline (Char.escaped key ^ ": " ^ s)

(* impure local function *)
let map_find key = AdjacentMap.find key !adjacent
let map_update key f = AdjacentMap.add key (f (map_find key)) !adjacent
let map_exists key = AdjacentMap.mem key !adjacent
let map_keys () = map (fun (k, v) -> k) (AdjacentMap.bindings !adjacent)
let map_values () = map (fun (k, v) -> v) (AdjacentMap.bindings !adjacent)
let map_random_key () = random_visit (map_keys ())
let map_print key = print_of_char_map key (AdjacentMap.find key !adjacent)

(* adjacent := AdjacentMap.add 's' ['f'] !adjacent ;; *)
(* AdjacentMap.iter print_of_char_map !adjacent ;; *)

(* adjacent := map_update 'q' (fun v -> []) ;; *)
(* AdjacentMap.iter print_of_char_map !adjacent ;; *)

(* adjacent := AdjacentMap.(!adjacent |> add 'q' ['g'; 'm'; 't']) ;; *)

let handle_token token = 
  let xs = explode token in 
  let process index key =
    let succ = nth xs (index + 1) in
    let store () = map_update key (fun v -> succ :: v) in
    let add () = AdjacentMap.(!adjacent |> add key [succ]) in  
    adjacent := (delta (map_exists key) store add) () in
    (* map_print key in *)
  iteri process (lizard xs) ;;

let handle_tokens xs = iter handle_token xs ;;

(* printf "%s " (string_of_char_pair (x, nth xs (index + 1))) *)
(* handleToken "mine" ;; *)


(* generator *)

handle_tokens [
  "galois"; 
  "euphoria"; 
] ;;

(* print_endline "---------------- data of map ----------------" ;; *)
(* AdjacentMap.iter print_of_char_map !adjacent ;; *)
print_endline "---------------- output ----------------" ;;

let next_grapheme peek = random_visit (map_find peek) ;;

(* rec token_builder : 
     [head] n -> [head, next]
  => [head, next] (n - 1) -> [head, next, next] 
  ... ... ... 
  => [head, next, ..., next] 1 -> xs *)
let rec token_builder xs len = 
  let cur = last xs in
  let found = map_exists cur in 
  match len == 1 || not found with | true -> xs 
    | false -> let xs' = push xs (next_grapheme cur) in 
      token_builder xs' (len - 1)

let next_token_fixed head len = fold_left glue "" (token_builder [head] len)
let next_token = next_token_fixed (map_random_key ())


(* filter predicate *)

let pred_aequilate len = fun token -> String.length token == len
let pred_degree_less n = fun token -> degree (explode token) <= n
let pred_degree_good = pred_degree_less 3



(* unsafe function due to possible endless recursion *)
(* notice: the adjacent should be large enough *)
(*
let rec next_filtered_fixed_unsafe head len predicate = 
  let candidate = next_token_fixed head len in 
  match predicate candidate with true -> candidate
    | false -> next_filtered_fixed_unsafe head len predicate

let next_filtered_unsafe = next_filtered_fixed_unsafe (map_random_key ())
let next_aequilate_unsafe len = 
let head = map_random_key () in
    print_string (Char.escaped head ^ ", ") ;
    next_filtered_fixed_unsafe (head) len (pred_aequilate len) ;;
*)

(* a better version with option *)
let rec next_filtered_fixed head len predicate = 
  let candidate = next_token_fixed head len in 
  match predicate candidate with true -> Some candidate
    | false -> None

let next_filtered = next_filtered_fixed (map_random_key ())
let next_aequilate len = 
  let head = map_random_key () in
  print_string (Char.escaped head ^ ", ") ;
  next_filtered_fixed (head) len (pred_aequilate len) ;;


for index = 1 to 5 do
  print_endline (case_opt "_____" (next_aequilate 5))
  (* print_endline (next_aequilate_unsafe 5) *)
  (* print_endline (next_token_fixed 'a' 5) *)
done ;;

let input = "中午吃啥" in
let len = String.length input in
let xs = init (len / 3) (fun i -> String.sub input (3 * i) 3) in
Printf.printf "%s" (String.concat "_" xs)





(*

(* quasi ad-hoc polymorphism in ocaml *)

let add_polymorphism = ((+), fun x y -> x ^ y) ;;

let (_, (+)) = add_polymorphism ;;
print_endline ("s" + "t") ;;

let ((+), _) = add_polymorphism ;;
print_int (1 + 3) ;;
print_newline () ;;

*)
