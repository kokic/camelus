
open List

let id x = x

let (++) xs s = String.concat s xs

let manifold f xs trans default = match xs with [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails

let manifold' f xs default = match xs with [] -> default
  | head :: tails -> fold_left f head tails

let comma = fun trans s t -> s ^ ", " ^ trans t

let show_set = fun xs -> match xs with [] -> "∅" 
  | _ -> "{" ^ xs ++ ", " ^ "}"
let show_list = fun xs -> "[" ^ xs ++ ", " ^ "]"

(* powerset of list *)


let rec powset = function
  | [] -> [[]]
  | x :: xs -> let ps = powset xs in 
    ps @ List.map (fun ss -> x :: ss) ps

let powset_tos xs = "{" ^ manifold comma xs show_set "∅" ^ "}"

;; 
let xs = powset ["a" ; "b"] in
print_endline (powset_tos xs)



