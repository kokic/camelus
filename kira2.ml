
let (=?) s = String.length s
let (|%|) s n = if n >= 0 then n else (=?) s + n
let subs s off pos = String.sub s off ((s |%| pos) - off + 1)
let subs' s pos = subs s 0 pos 
let drops n s = if n >= (=?) s then "" else subs s n ((=?) s - 1)
let (>>) s n = drops n s

let not_empty = (<>) String.empty

(* type ('token, 'value) gstate = State of ('value option * 'token) *)
type 'value state = State of ('value option * string)
type sstate = string state

let sstate_empty = State (None, String.empty)

(* type ('token, 'value) gparse = Parser of ('token -> ('value, 'token) gstate) *)
type 'value parser = Parser of (string -> 'value state)
type sparser = string parser

let parse p s = match p with Parser f -> f s

let return x source = State (Some x, source)
(* let string_head s = Char.escaped s.[0] *)

let token predicate = Parser (
  function "" -> sstate_empty
    | s when predicate s.[0] -> return s.[0] (s >> 1)
    | _ -> sstate_empty
)
let tokens n predicate: sparser = Parser (
  function "" -> sstate_empty
    | s when (=?) s < n -> sstate_empty
    | s -> let x = subs' s (n - 1) in if predicate x 
      then return x (s >> n) else sstate_empty
)
let token2 = tokens 2
let token3 = tokens 3

let inclusive n xs = tokens n (fun x -> (List.exists ((=) x) xs))
let inclusive2 = inclusive 2
let inclusive3 = inclusive 3


let exactly x = token ((==) x)
let exactly' s = tokens ((=?) s) ((==) s)


;; print_endline (exactly' "ssa" "ssawa")

(*  
  fun s -> if not_empty s && predicate s.[0] 
  then return (Char.escaped s.[0]) (s >> 1) else None *)
