
open List

let (++) xs s = String.concat s xs

let manifold f xs trans default = match xs with [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails

(* powerset of list *)

let commas = fun trans s t -> (s ++ ", ") ^ ", " ^ trans (t ++ ", ")

let rec powset = function
  | [] -> [[]]
  | x :: xs -> let ps = powset xs in 
    ps @ List.map (fun ss -> x :: ss) ps

;; 
(* let xs = manifold commas (powset ["0", "1"]) in *)
print_endline (", ") 



