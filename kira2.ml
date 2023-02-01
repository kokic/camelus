
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

type ('a, 'b, 'c) combinator = 'a parser -> 'b parser -> 'c parser
type scombinator = (string, string, string) combinator


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

let move a b = (a <&> b |> map) snd
let (->>) = move

let skip a b = (a <&> b |> map) fst
let (<<-) = skip

let either p p' = Parser (
  fun s -> match p <-- s with 
    | State (Some a, b) as x -> x
    | _ -> p' <-- s
)
let (<|>) = either


let map_pair f x = f (fst x) (snd x)
let extend f a b = (map (a <&> b) (map_pair f)) <|> a
let extend': scombinator = extend (^)



(* sparser *)

let asterisk p = Parser (
  fun s -> let rec aux buffer = function 
    | "" -> return buffer String.empty
    | residue -> match p <-- residue with 
      | State (Some a, b) -> aux (buffer ^ a) b
      | _ -> return buffer residue
  in aux String.empty s
)

let plus p = Parser (
  fun s -> match asterisk p <-- s with 
    | State (Some a, _) as x when not_empty a -> x
    | _ -> sstate_empty 
)

(* generator *)

let token predicate = Parser (
  function "" -> sstate_empty
    | s when predicate s.[0] -> return s.[0] (s >> 1)
    | _ -> sstate_empty
)
let token' predicate: sparser = map (token predicate) Char.escaped
let tokens n predicate: sparser = Parser (
  function "" -> sstate_empty
    | s when (=?) s < n -> sstate_empty
    | s -> let x = subs' s (n - 1) in if predicate x 
      then return x (s >> n) else sstate_empty
)
let token2 = tokens 2
let token3 = tokens 3

let includes xs = token' (fun x -> (List.exists ((==) x) xs))
let inclusive n xs = tokens n (fun x -> (List.exists ((=) x) xs))
let inclusive2 = inclusive 2
let inclusive3 = inclusive 3


let exactly x = token ((==) x)
let exactly' x = token' ((==) x)
let sexactly s = tokens ((=?) s) ((=) s)




let space = exactly' ' '
let spacea = space |> asterisk
let spaces = space |> plus

let soft a = spacea ->> a <<- spacea


let (-~) a b = fun x -> a <= x && x <= b

let digit = token' ('0' -~ '9')
let digits = digit |> plus


let letter = token' (fun x -> ('A' -~ 'Z') x || ('a' -~ 'z') x)
let letters = letter |> plus


let between left right a = left ->> a <<- right
let sides side = between side side

let operator x = exactly' x |> soft
let operators xs = includes xs |> soft




(* javascript *)

let underline_or_dollar = includes ['_'; '$']

let operator x: sparser = exactly' x |> soft
let operators xs: sparser = includes xs |> soft

let unary_prefix = operators ['!'; '~'; '+'; '-']
let incre_sides = inclusive2 ["++"; "--"] |> soft
let mul_infix = sexactly "**" <|> includes ['*'; '/'; '%'] |> soft
let add_infix = includes ['+'; '-']
let shift_infix = inclusive2 ["<<"; ">>"]
let eq_infix = inclusive2 ["=="; "!="]
let rel_infix = inclusive2 ["<="; ">="] <|> includes ['<'; '>']
let assign_infix = exactly' '=' 
  <|> inclusive3 ["<<="; ">>="; "**="]
  <|> inclusive2 ["&="; "|="; "^="; "+="; "-="; "*="; "/="; "%="]


let identifier_head = underline_or_dollar <|> letter
let identifier_body = letters <|> digits <|> underline_or_dollar |> asterisk
let identifier = extend' identifier_head identifier_body



let number = digits

let quotes = includes ['\''; '"']
let text_value = token' (fun x -> x != '\'' && x != '\"') |> asterisk
let text = sides quotes text_value

let primary_expr = identifier <|> number <|> text

let brackets p = between (operator '[') (operator ']') p

let ddot_accessor = operator '.' ->> identifier



let sof_ddot_ask p = map p ((^) "->")
let sof_brak_ask p = map p (fun x -> "->(" ^ x ^ ")" )

let glue p = map p (fun x -> fst x ^ snd x)

let rec expr = Parser (
  fun source -> 
    let member_expr_tail = ddot_accessor 
      <|> sof_brak_ask (brackets expr) |> plus in
    let member_expr = extend' primary_expr member_expr_tail in
    let rec unary_expr = Parser (
      fun source -> 
        let canonical = unary_prefix <&> unary_expr
          <|> (incre_sides <&> member_expr)
          <|> (member_expr <&> incre_sides) in 
        glue canonical <|> member_expr <-- source
    ) in
    unary_expr <-- source 
)


(* 
let rec unary_expr source = 
  let canonical = unary_prefix <&> unary_expr 
    <|> (incre_sides <&> member_expr)
    <|> (member_expr <&> incre_sides) in 
(glue canonical <|>  member_expr) source  
*)


;; print_sstate (expr <-- "+a[-b[!c[~d]]]")


