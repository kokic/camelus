
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


type archetype = COMMA 
               | DOT
               | COLON
               | SEMI
               | HOOK

               

type token = archetype * string
type tokens = token list

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



let any p f id = fun s -> 
  let rec aux accum = function 
    | "" -> return accum String.empty
    | residue -> match p <-- residue with 
      | State (Some a, b) -> aux (f accum a) b
      | _ -> return accum residue
  in aux id s



(* sparser *)

(* let asterisk (p: sparser): sparser = any p (^) String.empty *)

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
let add_infix = operators ['+'; '-']
let shift_infix = inclusive2 ["<<"; ">>"] |> soft
let rel_infix: sparser = inclusive2 ["<="; ">="] <|> includes ['<'; '>'] |> soft
let eq_infix = inclusive2 ["=="; "!="] |> soft
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
  | PropertyGet (x, y) -> string_of_expr x ^ "." ^ string_of_expr y
  | ElementGet (x, y) -> string_of_expr x ^ "[" ^ string_of_expr y ^ "]"
  | UnaryExpr (operator, x, prefix) -> let s = string_of_expr x in 
    if prefix then operator ^ s else s ^ operator
  | InfixExpr (i, x, y) -> string_of_expr x ^ " " ^ i ^ " " ^ string_of_expr y
  | _ -> "type<?>"

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


let ddot_access = operator '.' ->> identifier

let comma_of p = operator ',' ->> p

let lbracket = operator '['
let rbracket = operator ']'
let brackets p = between lbracket rbracket p

let parens p = between (operator '(') (operator ')') p

let one_some p p' f s = 
  match map2 (p <&> p') f <-- s with
    | State (None, _) as x -> x
    | State (Some a, b) -> any p' f a <-- b

let infix_lift p i = one_some p (i <&> p) 
  (fun x y -> InfixExpr (fst y, x, snd y)) <|> p



  (* and array_literal s = 
  let tail = 
      map (operator ']') (fun _ -> ([]: expr list))
  <|> map (eq_expr <<- operator ']') (fun x -> [x])
  <|> map2 (eq_expr <&> any (operator ',' ->> eq_expr) (fun x y -> x @ [y]) []  <<- operator ']') 
        (fun x xs -> x :: xs)
  in
  map (operator '[' ->> tail) (fun x -> ArrayLiteral x) <-- s *)

let rec expr s = eq_expr s

and array_literal s = 
  let rec_part = any (operator ',' ->> eq_expr) (fun x y -> x @ [y]) [] in
  let list_parser = map (lbracket ->> rbracket) (fun _ -> ([]: expr list))
    <|> map (brackets eq_expr) (fun x -> [x]) 
    <|> map2 (brackets (eq_expr <&> rec_part)) List.cons in
  map list_parser (fun x -> ArrayLiteral x) <-- s
and paren_expr s = map (parens expr) (fun x -> ParenthesizedExpr x) s 
and primary_expr s = 
      array_literal
  <|> paren_expr
  <|> identifier
  <|> number
  <|> text <-- s

and member_expr s = 
      one_some primary_expr ddot_access (fun x y -> PropertyGet (x, y))
  <|> one_some primary_expr (brackets expr) (fun x y -> ElementGet (x, y))
  <|> primary_expr <-- s

and unary_expr s = 
      map2 (unary_prefix <&> unary_expr) (fun x y -> UnaryExpr (x, y, true))
  <|> map2 (incre_sides <&> member_expr) (fun x y -> UnaryExpr (x, y, true))
  <|> map2 (member_expr <&> incre_sides) (fun x y -> UnaryExpr (y, x, false)) 
  <|> member_expr <-- s

and mul_expr s = (infix_lift unary_expr mul_infix) s
and add_expr s = (infix_lift mul_expr add_infix) s
and shift_expr s = (infix_lift add_expr shift_infix) s
and rel_expr s = (infix_lift shift_expr rel_infix) s
and eq_expr s = (infix_lift rel_expr eq_infix) s


(* 1 >> a + b [b.c * f + j][c.d / g - k] >= 1 == 'good' *)


let array_literal = 
  let rec_part = any (operator ',' ->> eq_expr) (fun x y -> x @ [y]) [] in
  let list_parser = map (lbracket ->> rbracket) (fun _ -> ([]: expr list))
    <|> map (brackets eq_expr) (fun x -> [x]) 
    <|> map2 (brackets (eq_expr <&> rec_part)) List.cons in
  map list_parser (fun x -> ArrayLiteral x)





let time f = let t = Sys.time () in
  let res = f () in Printf.printf "exec time: %fs\n"
    (Sys.time () -. t);
  res
;;

(* print_expr_state (time (fun () -> unary_expr "[[a]]")) *)

(* 
  OS: Manjaro Linux x86_64 
  CPU: 12th Gen Intel i7-12650H (16) @ 4.600GHz
  TIME: 
    * array_literal "[a == (b)]"
      - source: 0.06s
    * member_expr "[a == (b)]"
      - source: 0.18s
    * unary_expr "[a == (b)]"
      - source: 0.37s
    * expr "[a == (b)]"
      - source: 12s
      - bytecode: 11s
      - native: 1s
*)

(* ([] + [a]) + [a, b, c] + [a, b, d, e, f] *)




