open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Int _ | Bool _ | String _ -> e
  | ID var -> lookup env var
  | Not e ->
    (match eval_expr env e with
     | Bool b -> Bool (not b)
     | _ -> raise (TypeError "NOT error"))
  | Binop(op, e1, e2) ->
      (match op, eval_expr env e1, eval_expr env e2 with
      | Add, Int n1, Int n2 -> Int (n1 + n2)
      | Sub, Int n1, Int n2 -> Int (n1 - n2)
      | Mult, Int n1, Int n2 -> Int (n1 * n2)
      | Div, Int n1, Int n2 ->
          if n2 = 0 then raise (DivByZeroError)
          else Int (n1 / n2)
      | Add, _, _ | Sub, _, _ | Mult, _, _ | Div, _, _ -> raise (TypeError "Add/Sub/Mult/Div error")
      | Greater, Int n1, Int n2 -> Bool (n1 > n2)
      | Less, Int n1, Int n2 -> Bool (n1 < n2)
      | GreaterEqual, Int n1, Int n2 -> Bool (n1 >= n2)
      | LessEqual, Int n1, Int n2 -> Bool (n1 <= n2)
      | Greater, _, _ | Less, _, _ | GreaterEqual, _, _ | LessEqual, _, _ ->
        raise (TypeError ">/>=/</= error")
      | Concat, String s1, String s2 -> String (s1 ^ s2)
      | Equal, e1, e2 -> Bool (e1 = e2)
      | NotEqual, e1, e2 -> Bool (e1 <> e2)
      | And, Bool b1, Bool b2 -> Bool (b1 && b2)
      | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
      | Concat, _, _ -> raise (TypeError "Concat error")
      | Equal, _, _ | NotEqual, _, _ | And, _, _ | Or, _, _ -> raise (TypeError "=/<>/&&/|| error"))
  | If(e1, e2, e3) ->
      (match eval_expr env e1 with
      | Bool true -> eval_expr env e2
      | Bool false -> eval_expr env e3
      | _ -> raise (TypeError "If error"))
  | Let(var, flag, e1, e2) ->
      if flag then
        let v1 = extend env var (Int 0) in
        let v2 = eval_expr v1 e1 in
        update v1 var v2;  
        eval_expr v1 e2
      else
        let v1 = eval_expr env e1 in
        let new_env = extend env var v1 in
        eval_expr new_env e2
  | Fun(var, e1) ->
      Closure (env, var, e1)
  | App (e1, e2) ->
      let closure = eval_expr env e1 in
      let arg_val = eval_expr env e2 in
      (match closure with
      | Closure (closure_env, param, body) ->
          let new_env = extend closure_env param arg_val in
          eval_expr new_env body
      | _ -> raise (TypeError "Not a function"))
  | Record field_list ->
      Record (List.map (fun (Lab label, e1) -> (Lab label, eval_expr env e1)) field_list)
  | Select (Lab label, record_expr) ->
    (match eval_expr env record_expr with
    | Record fields ->
        (try List.assoc (Lab label) fields
         with Not_found -> raise (SelectError "Label not found"))
    | _ -> raise (TypeError "Not a record"))
  | Closure (_, _, _) as closure -> closure

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def(var, e1) ->
      let v1 = extend_tmp env var in
      let v2 = eval_expr v1 e1 in
      update v1 var v2;
      (v1, Some v2)
  | Expr e ->
      let v1 = eval_expr env e in
      (env, Some v1)
  | NoOp ->
      (env, None)
  | _ -> raise (TypeError "Not found")