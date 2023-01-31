

let return x source = Some (x, source)

let (=?) s = String.length s
let (|%|) s n = if n >= 0 then n else (=?) s + n
let subs s off pos = String.sub s off ((s |%| pos) - off + 1)
let subs' s pos = subs s 0 pos 
let drops n s = if n >= (=?) s then "" else subs s n ((=?) s - 1)
let (>>) s n = drops n s

let token predicate = fun s -> if predicate s.[0] 
  then return (Char.escaped s.[0]) (s >> 1) else None

let tokens n predicate = fun s -> let x = subs' s (n - 1) in if predicate x 
  then return x (s >> n) else None

let inclusive n xs = tokens n (fun x -> (List.exists ((=) x) xs))

let exactly x = token ((==) x)
let exactly' x = let n = (=?) x in fun s -> if x = subs' s (n - 1)
  then return x (s >> n) else None

let includes xs = token (fun x -> (List.exists ((==) x) xs))

let empty = "_"
let empty_pair = (empty, empty)
let empty_pair' = (empty_pair, empty)

let case o otherwise = match o with Some x -> x | _ -> otherwise
let case_pair o = case o empty_pair

let (&) f g x = f (g x)

let destruct x f = match x with Some (x1, x2) -> f x1 x2 | _ -> None

let map a f source = destruct (a source) (fun a1 a2 -> return (f a1) a2)

let asterisk a source = let rec aux buffer residue = 
  let pair = Some (buffer, residue) in 
  if (=?) residue > 0 then match a residue with 
    | Some (a1, a2) -> aux (buffer ^ a1) a2 
    | _ -> pair else pair
in aux "" source

let plus a source = match asterisk a source with 
  | Some (buffer, _) as pair when (=?) buffer > 0 -> pair 
  | _ -> None

let follow a b source = destruct (a source)
(fun a1 a2 -> destruct (b a2) (fun b1 b2 -> return (a1, b1) b2))
let (<&>) = follow

let move a b = (a <&> b |> map) snd
let (->>) = move

let either a b source = match a source with Some _ as x -> x | _ -> b source
let (<|>) = either

let skip a b source = destruct (a source)
(fun a1 a2 -> destruct (b a2) (fun b1 b2 -> return a1 b2))
let (<<-) = skip


let space = exactly ' '
let spacea = space |> asterisk
let spaces = space |> plus

let soft a = spacea ->> a <<- spacea


let (-~) a b = fun x -> a <= x && x <= b

let digit = token ('0' -~ '9')
let digits = digit |> plus


let letter = token (fun x -> ('A' -~ 'Z') x || ('a' -~ 'z') x)
let letters = letter |> plus


let map_tuple f x = f (fst x) (snd x)
let tuple_map f g x = (f (fst x), g (snd x))



(* javascript *)

let underline_or_dollar = includes ['_'; '$']

let operator x = exactly x |> soft
let operators xs = includes xs |> soft

let unary_prefix = operators ['!'; '^'; '+'; '-']
let incre_sides = inclusive 2 ["++"; "--"] |> soft
let mul_infix = exactly' "**" <|> includes ['*'; '/'; '%'] |> soft
let add_infix = includes ['+'; '-']
let shift_infix = inclusive 2 ["<<"; ">>"]
let eq_infix = inclusive 2 ["=="; "!="]
let rel_infix = inclusive 2 ["<="; ">="] <|> includes ['<'; '>']
let assign_infix = exactly '=' 
  <|> inclusive 3 ["<<="; ">>="; "**="]
  <|> inclusive 2 ["&="; "|="; "^="; "+="; "-="; "*="; "/="; "%="]

let identifier_head = underline_or_dollar <|> letter
let identifier_body = letters <|> digits <|> underline_or_dollar |> asterisk
let identifier = map (identifier_head <&> identifier_body) (map_tuple (^))

let number = digits

let quotes = includes ['\''; '"']
let text_value = token (fun x -> x != '\'' && x != '\"') |> asterisk
let text = quotes ->> text_value <<- quotes

let primary_expr = identifier <|> number <|> text



let member_expr_tail = operator '.' ->> identifier
  <|> (operator '[' ->> identifier <<- operator ']')
let member_expr = (map (primary_expr <&> member_expr_tail) (map_tuple (^)))
  <|> primary_expr

let expr = member_expr


let string_of_pair x y = ("('" ^ x ^ "', '" ^ y ^ "')")
let string_of_pair' x = string_of_pair (fst x) (snd x)
let fflat f x = let a = fst x in (f (fst a) (snd a), snd x)
let sfflat x = fflat (^) x

let print_pair x = print_endline (string_of_pair' x)



;; 
let pair = case (member_expr "1") empty_pair in
print_pair (pair)














