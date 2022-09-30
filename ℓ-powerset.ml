
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

let rec powset_ell ell xs = match ell with
  | 0 -> [] 
  | 1 -> [xs]
  | 2 -> powset xs
  | _ -> let prec = powset_ell (ell - 1) xs in 
         let succ = List.map (fun l -> powset l) prec 
         in concat succ



;; print_endline (powset_tos (powset_ell 2 ["a" ; "b"])) 
;; print_endline (powset_tos (powset_ell 3 ["a" ; "b"]))
;; print_endline (powset_tos (powset_ell 4 ["a" ; "b"]))




