
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
(* type 'value parser = Parser of (string -> 'value state) *)
type 'value parser = string -> 'value state
type sparser = string parser

type ('a, 'b, 'c) combinator = 'a parser -> 'b parser -> 'c parser
type scombinator = (string, string, string) combinator


let parse source = function p -> p source
let (<--) parser source = parse source parser

let map p f = fun s -> state_map (p <-- s) f

let store a state = state_map state (fun a' -> a, a')
let follow p p' = fun s -> match p <-- s with 
  | State (Some a, b) -> store a (p' <-- b)
  | _ -> sstate_empty
let (<&>) = follow

let map2 p f = map p (fun x -> f (fst x) (snd x))

let move a b = (a <&> b |> map) snd
let (->>) = move

let skip a b = (a <&> b |> map) fst
let (<<-) = skip

let either p p' = fun s -> match p <-- s with 
  | State (Some a, b) as x -> x
  | _ -> p' <-- s

let (<|>) a b = either a b


let extend f a b = (map2 (a <&> b) f) <|> a
let extend': scombinator = extend (^)



(* sparser *)

let asterisk (p: sparser): sparser = fun s -> 
  let rec aux buffer = function 
    | "" -> return buffer String.empty
    | residue -> match p <-- residue with 
      | State (Some a, b) -> aux (buffer ^ a) b
      | _ -> return buffer residue
  in aux String.empty s

let plus p: sparser = fun s -> match asterisk p <-- s with 
  | State (Some a, _) as x when not_empty a -> x
  | _ -> sstate_empty 


(* generator *)

let token predicate: char parser = 
  function "" -> sstate_empty
    | s when predicate s.[0] -> return s.[0] (s >> 1)
    | _ -> sstate_empty

let token' predicate: sparser = map (token predicate) Char.escaped
let tokens n predicate: sparser = 
  function "" -> sstate_empty
    | s when (=?) s < n -> sstate_empty
    | s -> let x = subs' s (n - 1) in if predicate x 
      then return x (s >> n) else sstate_empty

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
let incre_sides: sparser = inclusive2 ["++"; "--"] |> soft
let mul_infix: sparser = sexactly "**" <|> includes ['*'; '/'; '%'] |> soft
let add_infix = includes ['+'; '-']
let shift_infix = inclusive2 ["<<"; ">>"]
let eq_infix = inclusive2 ["=="; "!="]
let rel_infix: sparser = inclusive2 ["<="; ">="] <|> includes ['<'; '>']
let assign_infix: sparser = exactly' '=' 
  <|> inclusive3 ["<<="; ">>="; "**="]
  <|> inclusive2 ["&="; "|="; "^="; "+="; "-="; "*="; "/="; "%="]


let sof_ddot_ask p = map p ((^) "->")
let sof_brak_ask p = map p (fun x -> "->(" ^ x ^ ")" )

let glue p = map p (fun x -> fst x ^ snd x)


type expr = NumberLiteral of string
          | StringLiteral of string
          | Keywordliteral of string
          | Identifier of string
          | ArrayLiteral of expr list
          | ObjectLiteral of (expr * expr) list
          | ParenthesizedExpr of expr
          | RegExpLiteral of string

          | ElementGet of expr * expr
          | PropertyGet of expr * expr

          | FunctionExpr of string * params * expr
          | UnaryExpr of string * expr * bool
          | InfixExpr of string * expr * expr
          | ConditionalExpr of expr * expr * expr
          | FunctionCall of expr * params

          (* | NewExpr of newExpr *)

and params = expr list
(* and newExpr = functionCall * objectLiteral option *)


let string_of_array xs = "[" ^ String.concat ", " xs ^ "]"

let rec string_of_expr = function 
  | NumberLiteral n -> n
  | StringLiteral s -> "\"" ^ s ^ "\""
  | Keywordliteral k -> "<" ^ k ^ ">"
  | Identifier i -> i
  | ArrayLiteral xs -> string_of_array (List.map string_of_expr xs)
  | ParenthesizedExpr x -> "(" ^ string_of_expr x ^ ")"
  | UnaryExpr (operator, expr, prefix) -> 
    let s = string_of_expr expr in 
    if prefix then operator ^ s else s ^ operator
  | _ -> "_"

let print_expr_state = function
  | State (None, _) -> print_endline "error expr"
  | State (Some expr, s) -> let result = string_of_expr expr in 
    print_endline (string_of_pair result s)

let identifier_head = underline_or_dollar <|> letter
let identifier_body = letters <|> digits <|> underline_or_dollar |> asterisk
let identifier = map (extend' identifier_head identifier_body) 
  (fun x -> Identifier x)

let number = map digits (fun x -> NumberLiteral x)

let quotes = includes ['\''; '"']
let text_value = token' (fun x -> x != '\'' && x != '\"') |> asterisk
let text = map (sides quotes text_value) (fun x -> StringLiteral x)


let ddot_accessor = operator '.' ->> identifier
let brackets p = between (operator '[') (operator ']') p

let primary_expr = identifier <|> number <|> text

let property_get x y = PropertyGet (x, y)

let rec expr s = unary_expr s
and member_expr_tail = fun s -> ddot_accessor <|> brackets expr <-- s
and member_expr = fun s -> 
  (extend property_get primary_expr identifier) s
and unary_expr = fun s ->
        map2 (unary_prefix <&> unary_expr) (fun x y -> UnaryExpr (x, y, true))
    <|> map2 (incre_sides <&> member_expr) (fun x y -> UnaryExpr (x, y, true))
    <|> map2 (member_expr <&> incre_sides) (fun x y -> UnaryExpr (y, x, false)) 
    <|> member_expr <-- s




;; print_expr_state (expr "+++++a[-b[!c[~d]]]")


