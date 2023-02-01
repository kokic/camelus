
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

let return x residue = State (Some x, residue)

let state_map state f = match state with
  | State (Some a, b) -> return (f a) b
  | _ -> sstate_empty


let string_of_pair x y = ("('" ^ x ^ "', '" ^ y ^ "')")

let string_of_state f = function 
  | State (Some a, b) -> string_of_pair (f a) b
  | State (None, b) -> string_of_pair "_" b

let string_of_sstate: sstate -> string = string_of_state Fun.id

let print_sstate s = s |> string_of_sstate |> print_endline

(* type ('token, 'value) gparse = Parser of ('token -> ('value, 'token) gstate) *)
type 'value parser = Parser of (string -> 'value state)
type sparser = string parser

let parse source = function Parser p -> p source
let (<--) parser source = parse source parser

let map p f = Parser (fun s -> state_map (p <-- s) f)

let store a state = state_map state (fun a' -> a, a')
let follow p p' = Parser (
  fun s -> match p <-- s with 
    | State (Some a, b) -> store a (p' <-- b)
    | _ -> sstate_empty
)
let (<&>) = follow

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
let exactly' x = map (exactly x) Char.escaped

let sexactly s = tokens ((=?) s) ((=) s)

let glue p = map p (fun x -> fst x ^ snd x)

;; print_sstate (exactly' 's' <&> exactly' 's' |> glue <-- "ssr")


