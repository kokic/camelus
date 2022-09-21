
open List

let last xs = nth xs (length xs - 1)

let rec drop n = function [] -> []
  | head :: tails as xs -> match n with 0 -> xs | _ -> (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function [] -> []
  | head :: tails -> match n with 0 -> [] | _ -> head :: take (n - 1) tails
let lizard xs = take (length xs - 1) xs

let (=?) s = String.length s
let (-?) s x = String.index s x
let (|%|) s n = if n >= 0 then n else (=?) s + n
let (|+|) s n = s.[s |%| n]
let (++) xs s = String.concat s xs

let subs s off pos = String.sub s off ((s |%| pos) - off + 1)
let subs' s pos = subs s 0 pos 
let mids s = subs s 1 ((=?) s - 2)
let drops n s = if n >= (=?) s then "" else subs s n ((=?) s - 1)
let (>>) s n = drops n s
let explode s = init ((=?) s) (String.get s)

let split_ascii s s' = let head = s'.[0] in let ts = s' >> 1 in
  let n = (=?) ts in let accord = String.starts_with ~prefix: ts in
  let xs = String.split_on_char head s in let xs' = ref [hd xs] in
  let push_or_cat xs x = if accord x then xs @ [x >> n] 
    else lizard xs @ [last xs ^ Char.escaped head ^ x] in
  iter (fun x -> xs' := push_or_cat !xs' x) (tails xs); !xs'  

(* ;; print_endline (split_ascii "AppleOfTreeOfEarthInTheSpace" "Of" ++ ", ") *)

let explode_3char s = init ((=?) s / 3) (fun i -> subs s (3 * i) 3)
let explode_unicode = explode_3char

;; Printf.printf "%s" (explode_unicode "あいうえ" ++ "_")





