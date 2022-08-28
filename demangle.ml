
(*
                                    Perface
Inspiration by: 
  - https://android.googlesource.com/platform/external/gcc-demangle/+/
       d6f70187df96e4f1968adbe9b52035dc474b7590/cp-demangle.c
  - https://opensource.apple.com/source/gcc/gcc-5482/libiberty/cp-demangle.c.auto.html

Reference:
  - https://fitzgeraldnick.com/2017/02/22/cpp-demangle.html (in Rust)
  - https://itanium-cxx-abi.github.io/cxx-abi/abi.html (Itanium C++ ABI’s name mangling rules)
  - https://docs.oracle.com/javase/10/docs/api/com/sun/jdi/doc-files/signature.html
  - https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html

That's all, thanks! 

*)


(* basic util *)
open List

let delta condition x y = if condition then x else y

let last xs = nth xs (length xs - 1)
let push xs x = xs @ [x]

let (|->) s t = fun x -> delta (x == s) t x

let rec drop n = function [] -> []
  | head :: tails as xs -> match n with 0 -> xs | _ -> (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function [] -> []
  | head :: tails -> match n with 0 -> [] | _ -> head :: take (n - 1) tails
let lizard xs = take (length xs - 1) xs

let manifold f xs trans default = match xs with [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails


let case_opt default = function None -> default | Some x -> x
let case x = case_opt (failwith "case none") x  

let glue = fun trans s t -> s ^ trans t
let string_of_chars xs = manifold glue xs Char.escaped ""

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

(* demangle *)


(* JVM - JDI Type Signatures *)

(*  
  +---------------------------+-----------------------+
  | L fully-qualified-class ; | fully-qualified-class |
  +---------------------------+-----------------------+
  | [ type                    | type[]                |
  +---------------------------+-----------------------+
  | ( arg-types ) ret-type    | method type           |
  +---------------------------+-----------------------+
*)

module StringMap = Map.Make (String)

let jvm_mangle_encodings = StringMap . ( empty 
  |> add "Z" "boolean"
  |> add "B" "byte"
  |> add "C" "char"
  |> add "S" "short"
  |> add "I" "int"
  |> add "J" "long"
  |> add "F" "float"
  |> add "D" "double"
  |> add "V" "void"
)

let unknown = "__unknown" (* unknown type *)

let is_primitive s = StringMap.mem s jvm_mangle_encodings
let of_primitive s = match is_primitive s with
  true -> StringMap.find s jvm_mangle_encodings | _ -> unknown

let dotify s = string_of_chars (map ('/' |-> '.') (explode s))
let rec is_reference s = if (=?) s >= 2 then match s.[0] with
    'L' -> s |+| -1 == ';'
  | '[' -> let s' = drops 1 s in is_primitive s' || is_reference s'
  | ___ -> false
else false

let rec of_reference s = if is_reference s then match s.[0] with 
    'L' -> dotify (mids s)
  | '[' -> let s' = drops 1 s in 
           let d' = delta (is_reference s') in
               d' of_reference of_primitive s' ^ "[]"
  | ___ -> unknown
else unknown

let is_type s = is_primitive s || is_reference s
let of_type s = let case = match (=?) s with 
  1 -> of_primitive | _ -> of_reference in case s

let rec scan s xs = match (=?) s with 0 -> xs | _ -> 
  let consume t = scan (drops ((=?) t) s) (push xs (of_type t)) in 
  let rec trundle pos = let piece = subs s 0 pos in
    if piece |+| -1 == 'L' then subs s 0 (s -? ';') else
    if is_type piece then piece else trundle (pos + 1) in
  consume (trundle 0)
let scan' s = scan s [] 

let of_method s name = if s.[0] == '(' then 
  let pos = s -? ')' in
  let cat = String.concat ", " (scan' (subs s 1 (pos - 1))) in
  of_type (s >> pos + 1) ^ " " ^ name ^ "(" ^ cat ^ ")"
else unknown
let of_method' s = of_method s ""

(* ;; print_endline (of_method "([[IZ[Ljava/lang/Object;)V" "test") *)






let (-~) a b = fun x -> a <= x && x <= b

let locals_opt pred s = let n = (=?) s in 
  let rec trundle pos = if pos >= n then (n - 1) else 
    if pred s.[pos] then trundle (pos + 1) else pos - 1 in
  let pos = trundle 0 in 
  if pos >= 0 then Some (subs' s pos, pos) else None
let locals pred s = case_opt ("", -1) (locals_opt pred s)

let globals_opt pred s = let n = (=?) s in 
  let rec trundle pos = if pos >= n then None else 
    let piece = subs' s pos in
    if pred piece then Some (piece, pos) 
    else trundle (pos + 1) in 
  trundle 0
let globals pred s = case_opt ("", -1) (globals_opt pred s)


(* ;; print_endline (string_of_int (snd (locals ('0' -~ '9') "12E"))) *)
(* ;; print_endline (string_of_int (snd (globals (fun x -> String.equal x "aka") "akaE"))) *)





(* GCC - Itanium C++ ABI’s name mangling rules *)

let gcc_mangle_encodings = StringMap . ( empty 
  (* Compression *)
  |> add "St" "std::"
  |> add "Sa" "std::allocator"
  |> add "Sb" "std::basic_string"
  |> add "Ss" "std::string"
  |> add "Si" "std::istream"
  |> add "So" "std::ostream"
  |> add "Sd" "std::iostream"
  
  (* Builtin types *)
  |> add "v" "void"
  |> add "w" "wchar_t"
  |> add "b" "bool"

  |> add "c" "char" |> add "a" "signed char" |> add "h" "unsigned char"
  |> add "s" "short" |> add "t" "unsigned short"
  |> add "i" "int" |> add "j" "unsigned int"
  |> add "l" "long" |> add "m" "unsigned long"
  |> add "x" "long long" |> add "y" "unsigned long long"
  |> add "n" "__int128" |> add "o" "unsigned __int128"
  |> add "f" "float"
  |> add "d" "double"
  |> add "e" "long double"
  |> add "g" "__float128" (* __float80 *)
  |> add "z" "ellipsis" (* ... *)
)

let gcc_is_alias s = StringMap.mem s gcc_mangle_encodings
let gcc_of_alias s = case_opt unknown (StringMap.find_opt s gcc_mangle_encodings)

let rec gcc_breaker xs n s =  
  let piece, pos = locals ('0' -~ '9') s in  
  if pos < 0 then (xs, n) else 
    let anchor = pos + int_of_string piece in    
    let xs' = push xs (subs s (pos + 1) anchor) in
    let s' = s >> anchor + 1 in
    gcc_breaker xs' (n + anchor + (=?) piece) s' 
let gcc_breaker' = gcc_breaker [] 0

let gcc_cut s = let xs, n = gcc_breaker' s in xs ++ "::", s >> n
let gcc_name_cut s = gcc_cut (if s.[0] == 'N' then s >> 1 else s)

type stat = Normal 
  | Pointer 
  | Reference (* left *) | RReference (* right *)
  | Const

let gcc_status s status = 
  let single stat = match stat with
    | Pointer -> "*" 
    | Reference -> "&" 
    | RReference -> "&&"
    | Const -> " const"
    | _ -> "" in
  s ^ map single (rev status) ++ ""

let rec gcc_of_params xs s status = if (=?) s <= 0 then xs else
  let infl name = gcc_status name status in
  let record_cut name surplus = gcc_of_params (push xs (infl name)) surplus [] in
  let record_status stat = gcc_of_params xs (s >> 1) (push status stat) in
  let head = s.[0] in match head with
    | 'N' -> let name, surplus = gcc_cut (s >> 1) in record_cut name surplus
    | 'E' -> gcc_of_params xs (s >> 1) []
    | 'P' -> record_status Pointer
    | 'R' -> record_status Reference
    | 'O' -> record_status RReference
    | 'K' -> record_status Const
    | ___ -> let (piece, pos) = globals gcc_is_alias s in
             if pos < 0 then let name, surplus = gcc_cut s in
             if (=?) name <= 0 then xs else record_cut name surplus else 
             let name = gcc_of_alias piece in record_cut name (s >> pos + 1)
let gcc_of_params' s = gcc_of_params [] s []

let gcc_demangle s = if String.starts_with ~prefix: "_Z" s then
  let s' = if s |+| -1 == 'v' then subs' s (-2) else s in 
  let name, surplus = gcc_name_cut (s' >> 2) in
  let xs = gcc_of_params' surplus in
  name ^ "(" ^ xs ++ ", " ^ ")" 
else s

;; print_endline (gcc_demangle "_ZN6Player17setPlayerGameTypeE8GameType")
;; print_endline (gcc_demangle "_ZN6Player14setCarriedItemERK12ItemInstance")
;; print_endline (gcc_demangle "_ZN6Player7setNameERKSs")
;; print_endline (gcc_demangle "_ZN6Player8setArmorE9ArmorSlotRK12ItemInstance")




(* 
                              Compression

   <substitution> ::= St # ::std::
   <substitution> ::= Sa # ::std::allocator
   <substitution> ::= Sb # ::std::basic_string
   <substitution> ::= Ss # ::std::basic_string < char, 
                                                 ::std::char_traits<char>, 
                                                 ::std::allocator<char> >
   <substitution> ::= Si # ::std::basic_istream<char,  std::char_traits<char> >
   <substitution> ::= So # ::std::basic_ostream<char,  std::char_traits<char> >
   <substitution> ::= Sd # ::std::basic_iostream<char, std::char_traits<char> >
*)

(*
              Builtin types

  <builtin-type> 
     ::= v  # void
     ::= w  # wchar_t
     ::= b  # bool
     ::= c  # char
     ::= a  # signed char
     ::= h  # unsigned char
     ::= s  # short
     ::= t  # unsigned short
     ::= i  # int
     ::= j  # unsigned int
     ::= l  # long
     ::= m  # unsigned long
     ::= x  # long long, __int64
     ::= y  # unsigned long long, __int64
     ::= n  # __int128
     ::= o  # unsigned __int128
     ::= f  # float
     ::= d  # double
     ::= e  # long double, __float80
     ::= g  # __float128
     ::= z  # ellipsis
     ::= Dd # IEEE 754r decimal floating point (64 bits)
     ::= De # IEEE 754r decimal floating point (128 bits)
     ::= Df # IEEE 754r decimal floating point (32 bits)
     ::= Dh # IEEE 754r half-precision floating point (16 bits)
     ::= DF <number> _ # ISO/IEC TS 18661 binary floating point type _FloatN (N bits)
     ::= DB <number> _        # C23 signed _BitInt(N)
     ::= DB <instantiation-dependent expression> _ # C23 signed _BitInt(N)
     ::= DU <number> _        # C23 unsigned _BitInt(N)
     ::= DU <instantiation-dependent expression> _ # C23 unsigned _BitInt(N)
     ::= Di # char32_t
     ::= Ds # char16_t
     ::= Du # char8_t
     ::= Da # auto
     ::= Dc # decltype(auto)
     ::= Dn # std::nullptr_t (i.e., decltype(nullptr))
     ::= u <source-name> [<template-args>] # vendor extended type
*)


(*
                       Type encodings

  <type> ::= <builtin-type>
         ::= <qualified-type>
         ::= <function-type>
         ::= <class-enum-type>
         ::= <array-type>
         ::= <pointer-to-member-type>
         ::= <template-param>
         ::= <template-template-param> <template-args>
         ::= <decltype>
         ::= P <type>        # pointer
         ::= R <type>        # l-value reference
         ::= O <type>        # r-value reference (C++11)
         ::= C <type>        # complex pair (C99)
         ::= G <type>        # imaginary (C99)
         ::= <substitution>  # See Compression below
*)

(* 
    Operator Encodings:

<operator-name> ::= nw  # new           
      ::= na  # new[]
      ::= dl  # delete        
      ::= da  # delete[]      
      ::= aw  # co_await      
      ::= ps  # + (unary)
      ::= ng  # - (unary)     
      ::= ad  # & (unary)     
      ::= de  # * (unary)     
      ::= co  # ~             
      ::= pl  # +             
      ::= mi  # -             
      ::= ml  # *             
      ::= dv  # /             
      ::= rm  # %             
      ::= an  # &             
      ::= or  # |             
      ::= eo  # ^             
      ::= aS  # =             
      ::= pL  # +=            
      ::= mI  # -=            
      ::= mL  # *=            
      ::= dV  # /=            
      ::= rM  # %=            
      ::= aN  # &=            
      ::= oR  # |=            
      ::= eO  # ^=            
      ::= ls  # <<            
      ::= rs  # >>            
      ::= lS  # <<=           
      ::= rS  # >>=           
      ::= eq  # ==            
      ::= ne  # !=            
      ::= lt  # <             
      ::= gt  # >             
      ::= le  # <=            
      ::= ge  # >=            
      ::= ss  # <=>           
      ::= nt  # !             
      ::= aa  # &&            
      ::= oo  # ||            
      ::= pp  # ++ (postfix in <expression> context)
      ::= mm  # -- (postfix in <expression> context)           
      ::= cm  # ,             
      ::= pm  # ->*           
      ::= pt  # ->            
      ::= cl  # ()            
      ::= ix  # []            
      ::= qu  # ?             
      ::= cv <type>  # (cast)
      ::= li <source-name>        # operator ""
      ::= v <digit> <source-name>  # vendor extended operator
*)

(*                 
               Virtual Tables and RTTI

<special-name> ::= TV <type>  # virtual table
     ::= TT <type>  # VTT structure (construction vtable index)
     ::= TI <type>  # typeinfo structure
     ::= TS <type>  # typeinfo name (null-terminated byte string)
*)





