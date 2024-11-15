open Types

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let sanitize_string str =
  if String.length str >= 2 then
    let first_char = String.get str 0 in
    let last_char = String.get str (String.length str - 1) in
    if first_char = '\"' && last_char = '\"' then
      String.sub str 1 (String.length str - 2)
    else
      raise (InvalidInputException "sanitize error")
  else
    raise (InvalidInputException "sanitize error")

let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lcurly = Str.regexp "{"
let re_rcurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not\\b"
let re_if = Str.regexp "if\\b"
let re_then = Str.regexp "then\\b"
let re_else = Str.regexp "else\\b"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let\\b"
let re_def = Str.regexp "def\\b"
let re_in = Str.regexp "in\\b"
let re_rec = Str.regexp "rec\\b"
let re_fun = Str.regexp "fun\\b"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_semi = Str.regexp ";"
let re_bool = Str.regexp "true\\|false"
let re_positive_int = Str.regexp "[0-9]+"
let re_negative_int = Str.regexp "(-[0-9]+)"
let re_string = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_tab = Str.regexp "\\\t"
let re_newline = Str.regexp "\\\n"
let re_space = Str.regexp " "

let tokenize input = 
  let rec tok pos s = 
    if pos >= String.length s then []
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rcurly s pos then Tok_RCurly :: tok (pos + 1) s
    else if Str.string_match re_lcurly s pos then Tok_LCurly :: tok (pos + 1) s
    else if Str.string_match re_dot s pos then Tok_Dot :: tok (pos + 1) s
    else if Str.string_match re_equal s pos then Tok_Equal :: tok (pos + 1) s
    else if Str.string_match re_notequal s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_NotEqual :: tok (pos + len) s
    else if Str.string_match re_greater s pos then Tok_Greater :: tok (pos + 1) s
    else if Str.string_match re_less s pos then Tok_Less :: tok (pos + 1) s
    else if Str.string_match re_greaterequal s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_GreaterEqual :: tok (pos + len) s
    else if Str.string_match re_lessequal s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_LessEqual :: tok (pos + len) s
    else if Str.string_match re_bool s pos then
      let tok_str = Str.matched_string s in
      let token = if tok_str = "true" then Tok_Bool true else Tok_Bool false in
      let len = String.length tok_str in 
      token :: tok (pos + len) s
    else if Str.string_match re_positive_int s pos then
      let tok_str = Str.matched_string s in
      let token = Tok_Int (int_of_string tok_str) in
      let len = String.length tok_str in 
      token :: tok (pos + len) s
    else if Str.string_match re_negative_int s pos then
      let tok_str = Str.matched_string s in
      let num_str = String.sub tok_str 1 (String.length tok_str - 2) in
      let num = int_of_string num_str in
      let token = Tok_Int num in
      let len = String.length tok_str in 
      token :: tok (pos + len) s
    else if Str.string_match re_string s pos then
      let tok_str = Str.matched_string s in
      let modified = sanitize_string tok_str in
      let token = Tok_String modified in
      let len = String.length tok_str in 
      token :: tok (pos + len) s
    else if Str.string_match re_arrow s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Arrow :: tok (pos + len) s
    else if Str.string_match re_or s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Or :: tok (pos + len) s
    else if Str.string_match re_and s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_And :: tok (pos + len) s
    else if Str.string_match re_not s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Not :: tok (pos + len) s
    else if Str.string_match re_if s pos then Tok_If :: tok (pos + 2) s
    else if Str.string_match re_then s pos then Tok_Then :: tok (pos + 4) s
    else if Str.string_match re_else s pos then Tok_Else :: tok (pos + 4) s
    else if Str.string_match re_add s pos then Tok_Add :: tok (pos + 1) s
    else if Str.string_match re_sub s pos then Tok_Sub :: tok (pos + 1) s
    else if Str.string_match re_mult s pos then Tok_Mult :: tok (pos + 1) s
    else if Str.string_match re_div s pos then Tok_Div :: tok (pos + 1) s
    else if Str.string_match re_doublesemi s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_DoubleSemi :: tok (pos + len) s
    else if Str.string_match re_let s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Let :: tok (pos + len) s
    else if Str.string_match re_def s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Def :: tok (pos + len) s
    else if Str.string_match re_in s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_In :: tok (pos + len) s
    else if Str.string_match re_rec s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Rec :: tok (pos + len) s
    else if Str.string_match re_fun s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Fun :: tok (pos + len) s
    else if Str.string_match re_semi s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_Semi :: tok (pos + len) s
    else if Str.string_match re_concat s pos then Tok_Concat :: tok (pos + 1) s
    else if Str.string_match re_tab s pos || Str.string_match re_newline s pos || Str.string_match re_space s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      tok (pos + len) s
    else if Str.string_match re_id s pos then
      let token = Str.matched_string s in
      let len = String.length token in 
      Tok_ID token :: tok (pos + len) s  
    else raise (InvalidInputException "tokenize error")
in
tok 0 input