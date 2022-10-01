
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

let rec ellps ell xs = match ell with
  | 0 -> [] 
  | 1 -> [xs]
  | 2 -> powset xs
  | _ -> if ell < 0 && ell mod 2 == 0 then ellps (-ell) xs
         else let prec = ellps (ell - 1) xs in 
         concat (List.map (fun l -> powset l) prec)
      
let print_ellps ell src = let xs = ellps ell src in 
  print_endline (powset_tos xs ^ ": " ^ string_of_int(length xs)) 

let print_ellps_ab ell = print_ellps ell ["a" ; "b"]

;; print_ellps_ab (-3)
;; print_ellps_ab (-5)


