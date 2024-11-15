open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of toks given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let print_token = function
  | Tok_ID s -> Printf.printf "Tok_ID \"%s\"\n" s
  | Tok_Int i -> Printf.printf "Tok_Int %d\n" i
  | Tok_Bool b -> Printf.printf "Tok_Bool %b\n" b
  | Tok_String s -> Printf.printf "Tok_String \"%s\"\n" s
  | Tok_Let -> print_endline "Tok_Let"
  | Tok_In -> print_endline "Tok_In"
  | Tok_If -> print_endline "Tok_If"
  | Tok_Then -> print_endline "Tok_Then"
  | Tok_Else -> print_endline "Tok_Else"
  | Tok_Fun -> print_endline "Tok_Fun"
  | Tok_Rec -> print_endline "Tok_Rec"
  | Tok_Add -> print_endline "Tok_Add"
  | Tok_Sub -> print_endline "Tok_Sub"
  | Tok_Mult -> print_endline "Tok_Mult"
  | Tok_Div -> print_endline "Tok_Div"
  | Tok_Equal -> print_endline "Tok_Equal"
  | Tok_NotEqual -> print_endline "Tok_NotEqual"
  | Tok_Less -> print_endline "Tok_Less"
  | Tok_Greater -> print_endline "Tok_Greater"
  | Tok_LessEqual -> print_endline "Tok_LessEqual"
  | Tok_GreaterEqual -> print_endline "Tok_GreaterEqual"
  | Tok_And -> print_endline "Tok_And"
  | Tok_Or -> print_endline "Tok_Or"
  | Tok_Not -> print_endline "Tok_Not"
  | Tok_Arrow -> print_endline "Tok_Arrow"
  | Tok_DoubleSemi -> print_endline "Tok_DoubleSemi"
  | Tok_Semi -> print_endline "Tok_Semi"
  | Tok_LParen -> print_endline "Tok_LParen"
  | Tok_RParen -> print_endline "Tok_RParen"
  | Tok_LCurly -> print_endline "Tok_LCurly"
  | Tok_RCurly -> print_endline "Tok_RCurly"
  | Tok_Dot -> print_endline "Tok_Dot"
  | Tok_Concat -> print_endline "Tok_Concat"
  | Tok_Def -> print_endline "Tok_Def"

let print_token_list tokens =
  List.iter print_token tokens

let print_lookahead toks =
  match lookahead toks with
  | Some token -> 
      print_string "Next token: "; 
      print_token token
  | None -> 
      print_endline "No more tokens"

let rec parse_expr toks = 
  (*print_endline "Parsing expression with tokens:";
  print_lookahead toks;*)
  match lookahead toks with
  | Some Tok_Let -> parse_let_expr toks
  | Some Tok_If -> parse_if_expr toks
  | Some Tok_Fun -> parse_function_expr toks
  | _ -> parse_or_expr toks

and parse_let_expr toks =
  (*print_endline "Parsing expression with tokens:";
  print_token_list toks;*)
  let toks = match_token toks Tok_Let in
  let flag, toks = match lookahead toks with
    | Some Tok_Rec -> (true, match_token toks Tok_Rec)
    | _ -> (false, toks) in
  let var, toks = match lookahead toks with
    | Some Tok_ID id -> (id, match_token toks (Tok_ID id))
    | _ -> raise (InvalidInputException "Expected identifier") in
  let toks = match_token toks Tok_Equal in
  let (toks, e1) = parse_expr toks in
  let toks = match_token toks Tok_In in
  let (toks, e2) = parse_expr toks in
  (toks, Let(var, flag, e1, e2))

and parse_if_expr toks =
  let toks = match_token toks Tok_If in
  let (toks, e1) = parse_expr toks in
  let toks = match_token toks Tok_Then in
  let (toks, e2) = parse_expr toks in
  let toks = match_token toks Tok_Else in
  let (toks, e3) = parse_expr toks in
  (toks, If(e1, e2, e3))

and parse_function_expr toks =
  (*print_endline "Parsing expression with tokens:";
  print_token_list toks;*)
  let toks = match_token toks Tok_Fun in
  let var, toks = match lookahead toks with
    | Some Tok_ID id -> (id, match_token toks (Tok_ID id))
    | _ -> raise (InvalidInputException "Expected identifier") in
  let toks = match_token toks Tok_Arrow in
  let (toks, e1) = parse_expr toks in
  (toks, Fun(var, e1))

and parse_or_expr toks =
  let toks, e1 = parse_and_expr toks in
  match lookahead toks with 
  | Some Tok_Or ->
      let toks = match_token toks Tok_Or in
      let toks, e2 = parse_or_expr toks in
      (toks, Binop(Or, e1, e2))
  | _ -> (toks, e1)

and parse_and_expr toks = 
  let toks, e1 = parse_equality_expr toks in
  match lookahead toks with
  | Some Tok_And ->
      let toks = match_token toks Tok_And in
      let toks, e2 = parse_and_expr toks in
      (toks, Binop(And, e1, e2))
  | _ -> (toks, e1)

and parse_equality_expr toks = 
  let toks, e1 = parse_relational_expr toks in
  match lookahead toks with
  | Some Tok_Equal ->
    let toks = match_token toks Tok_Equal in
    let toks, e2 = parse_equality_expr toks in
    (toks, Binop(Equal, e1, e2))
  | Some Tok_NotEqual ->
    let toks = match_token toks Tok_NotEqual in
    let toks, e2 = parse_equality_expr toks in
    (toks, Binop(NotEqual, e1, e2))
  | _ -> (toks, e1)

and parse_relational_expr toks =
  let toks, e1 = parse_additive_expr toks in
  match lookahead toks with
  | Some Tok_Greater ->
      let toks = match_token toks Tok_Greater in
      let toks, e2 = parse_relational_expr toks in
      (toks, Binop(Greater, e1, e2))
  | Some Tok_GreaterEqual ->
      let toks = match_token toks Tok_GreaterEqual in
      let toks, e2 = parse_relational_expr toks in
      (toks, Binop(GreaterEqual, e1, e2))
  | Some Tok_Less ->
      let toks = match_token toks Tok_Less in
      let toks, e2 = parse_relational_expr toks in
      (toks, Binop(Less, e1, e2))
  | Some Tok_LessEqual ->
      let toks = match_token toks Tok_LessEqual in
      let toks, e2 = parse_relational_expr toks in
      (toks, Binop(LessEqual, e1, e2))
  | _ -> (toks, e1)

and parse_additive_expr toks =
  let toks, e1 = parse_multiplicative_expr toks in
  match lookahead toks with
  | Some Tok_Add ->
    let toks = match_token toks Tok_Add in
    let toks, e2 = parse_additive_expr toks in
    (toks, Binop(Add, e1, e2))
  | Some Tok_Sub ->
    let toks = match_token toks Tok_Sub in
    let toks, e2 = parse_additive_expr toks in
    (toks, Binop(Sub, e1, e2))
  | _ -> (toks, e1)

and parse_multiplicative_expr toks = 
  let toks, e1 = parse_concat_expr toks in
  match lookahead toks with
  | Some Tok_Mult ->
    let toks = match_token toks Tok_Mult in
    let toks, e2 = parse_multiplicative_expr toks in
    (toks, Binop(Mult, e1, e2))
  | Some Tok_Div ->
    let toks = match_token toks Tok_Div in
    let toks, e2 = parse_multiplicative_expr toks in
    (toks, Binop(Div, e1, e2))
  | _ -> (toks, e1)

and parse_concat_expr toks = 
  let toks, e1 = parse_unary_expr toks in
  match lookahead toks with
  | Some Tok_Concat ->
    let toks = match_token toks Tok_Concat in
    let toks, e2 = parse_concat_expr toks in
    (toks, Binop(Concat, e1, e2))
  | _ -> (toks, e1)

and parse_unary_expr toks = 
  match lookahead toks with
  | Some Tok_Not ->
      let toks = match_token toks Tok_Not in
      let toks, e1 = parse_unary_expr toks in
      (toks, Not(e1))
  | _ -> parse_app_expr toks

and parse_app_expr toks =
  let toks, e1 = parse_select_expr toks in
  match lookahead toks with 
  | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen | Some Tok_LCurly ->
      let toks, e2 = parse_primary_expr toks in
      (toks, App(e1, e2))
  | _ -> (toks, e1)

and parse_select_expr toks = 
  let toks, e1 = parse_primary_expr toks in
  match lookahead toks with 
  | Some Tok_Dot ->
      let toks = match_token toks Tok_Dot in
      (match lookahead toks with
      | Some Tok_ID id ->
          let toks = match_token toks (Tok_ID id) in
          (toks, Select(Lab id, e1))
      | _ -> raise (InvalidInputException "Expected identifier"))
  | _ -> (toks, e1)

and parse_primary_expr toks =
  match lookahead toks with
  | Some Tok_Int n ->
      let toks = match_token toks (Tok_Int n) in
      (toks, Int(n))
  | Some Tok_Bool b ->
      let toks = match_token toks (Tok_Bool b) in
      (toks, Bool(b))
  | Some Tok_String s ->
      let toks = match_token toks (Tok_String s) in
      (toks, String s)
  | Some Tok_ID id ->
      let toks = match_token toks (Tok_ID id) in
      (toks, ID id)
  | Some Tok_LParen ->
      let toks = match_token toks Tok_LParen in
      let toks, e1 = parse_expr toks in
      let toks = match_token toks Tok_RParen in
      (toks, e1)
  | Some Tok_LCurly -> parse_record_expr toks 
  | _ -> raise (InvalidInputException "Unexpected token")

and parse_record_expr toks = 
  let toks = match_token toks Tok_LCurly in
  let toks, e1 = parse_record_body_expr toks in
  let toks = match_token toks Tok_RCurly in
  (toks, Record e1)

and parse_record_body_expr toks =
  match lookahead toks with
  | Some Tok_RCurly -> (toks, []) 
  | Some Tok_ID id ->
      let toks = match_token toks (Tok_ID id) in  
      let toks = match_token toks Tok_Equal in  
      let toks, e1 = parse_expr toks in  
      let pair = (Lab id, e1) in  
      (match lookahead toks with
      | Some Tok_Semi ->
          let toks = match_token toks Tok_Semi in  
          let (toks, e2) = parse_record_body_expr toks in  
          (toks, pair :: e2)  
      | _ ->
          let (toks, e2) = parse_record_body_expr toks in
          (toks, pair :: e2))  
  | _ -> raise (InvalidInputException "Expected indentifier")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_DoubleSemi ->
    let toks = match_token toks Tok_DoubleSemi in 
    (toks, NoOp) 
  | Some Tok_Def ->
      let toks = match_token toks Tok_Def in 
      (match lookahead toks with
       | Some Tok_ID id -> 
           let toks = match_token toks (Tok_ID id) in 
           let toks = match_token toks Tok_Equal in 
           let (toks, e1) = parse_expr toks in 
           let toks = match_token toks Tok_DoubleSemi in
           (toks, Def (id, e1)) 
       | _ -> raise (InvalidInputException "Expected identifier"))
  | _ ->
      let (toks, e1) = parse_expr toks in 
      let toks = match_token toks Tok_DoubleSemi in 
      (toks, Expr e1) 
