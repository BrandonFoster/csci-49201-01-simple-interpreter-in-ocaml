(* Common-Lisp Evaluation *)
open CommonLisp

exception EvalError of string

type eval_type =
  | EvalInt of int
  | EvalRatio of (int * int)
  | EvalString of string
  | EvalChar of char
  | EvalNil

module Env = struct
  type vars = 
    { var: (symbol * eval_type) option;
      next: vars option }

  type environment =
    { global_vars: vars;
      local_vars: vars } 

  let make g_vars l_vars =
    { global_vars = g_vars; local_vars = l_vars }

  let update env env2 =
    make env2.global_vars env.local_vars

  let empty =
    let empty_vars = { var = None; next = None } in
    make empty_vars empty_vars
  
  let bind_var vars symbol value =
    { var = Some (symbol, value); next = Some vars }

  let bind_global env symbol value =
    { global_vars = bind_var env.global_vars symbol value; local_vars = env.local_vars }

  let bind_local env symbol value =
    { global_vars = env.global_vars; local_vars = bind_var env.local_vars symbol value }

  let rec get_value vars symbol =
    let next = function
      | Some next_vars -> get_value next_vars symbol
      | _ -> raise (EvalError "SYMBOL-error")
    in
    match vars.var with
    | Some (sym, value) -> if sym = symbol then value else next vars.next
    | _ -> raise (EvalError "SYMBOL-error")

  let get_global_value env symbol =
    get_value env.global_vars symbol

  let get_local_value env symbol =
    get_value env.local_vars symbol
end

type eval_return =
  | EvalReturn of (Env.environment * eval_type)

let get_env = function
  | EvalReturn (env, _) -> env

let get_eval = function
  | EvalReturn (_, ev) -> ev

let eval_ret_update old_env = function
  | EvalReturn (env, ev) -> EvalReturn ((Env.update old_env env), ev)

(* Helper Functions *)
let rec gcd a b =
  if 0 = b then
    a
  else
    gcd b (a mod b)

(* Lisp Evaluation Functions *)
let rec lisp_print env expr =
  if 1 = cons_length expr then
    begin match eval_expr env (car expr) with
    | EvalReturn (env2, EvalInt i) ->
        Printf.printf "%i\n" i;
        EvalReturn ((Env.update env env2), EvalInt i)
    | EvalReturn (env2, EvalRatio (i1, i2)) ->
        Printf.printf "%i/%i\n" i1 i2;
        EvalReturn ((Env.update env env2), EvalRatio (i1, i2))
    | EvalReturn (env2, EvalString str) ->
        Printf.printf "\"%s\"\n" str;
        EvalReturn ((Env.update env env2), EvalString str)
    | EvalReturn (env2, EvalChar c) ->
        Printf.printf "%s\n" ("#\\"^(Char.escaped c));
        EvalReturn ((Env.update env env2), EvalChar c)
    | EvalReturn (env2, EvalNil) ->
        Printf.printf "NIL\n";
        EvalReturn ((Env.update env env2), EvalNil)
    end
  else
    raise (EvalError "PRINT-error")
and lisp_setq env expr =
  match (car expr), (eval_expr env (car (cdr (expr)))) with
  | Leaf (Symbol str), EvalReturn (env2, ev) ->
      begin let env3 = Env.bind_global env2 str ev in
      if 2 < cons_length expr then
        eval_ret_update env (lisp_setq env3 (cdr (cdr (expr))))
      else
        EvalReturn ((Env.update env env3), (Env.get_global_value env3 str))
      end
  | _ ->
      raise (EvalError "SETQ-error")
and lisp_dotimes env expr =
  let rec loop env var count ret body curr =
    let env2 = Env.bind_local env var (EvalInt curr) in
    if count <= 0 || curr >= count then
      begin match eval_expr env2 ret with
      | EvalReturn (env3, ev) -> EvalReturn ((Env.update env env3), ev)
      end
    else
      begin match eval env2 body with
      | EvalReturn (env3, ev) -> loop (Env.update env env3) var count ret body (curr + 1)
      end
  in
  match (car expr), (cdr expr) with
  | params, body ->
      begin if 2 = cons_length params then
        begin match (car params), (eval_expr env (car (cdr params))) with
        | Leaf Symbol sym, EvalReturn (env2, EvalInt i) ->
            loop (Env.update env env2) sym i (Leaf Nil) body 0
        | Leaf Symbol sym, EvalReturn (env2, EvalRatio (i1, i2)) ->
            let count = int_of_float (ceil ((float_of_int i1) /. (float_of_int i2))) in
            loop (Env.update env env2) sym count (Leaf Nil) body 0
        | _ ->
            raise (EvalError "DOTIMES-error")
        end
      else if 3 = cons_length params then
        begin match (car params), (eval_expr env (car (cdr params))), (car (cdr (cdr params))) with
        | Leaf Symbol sym, EvalReturn (env2, EvalInt i), ret ->
            loop (Env.update env env2) sym i ret body 0
        | Leaf Symbol sym, EvalReturn (env2, EvalRatio (i1, i2)), ret ->
            let count = int_of_float (ceil ((float_of_int i1) /. (float_of_int i2))) in
            loop (Env.update env env2) sym count ret body 0
        | _ ->
            raise (EvalError "DOTIMES-error")
        end
      else
        raise (EvalError "DOTIMES-error")
      end
and lisp_add env expr =
  let result env val1 val2 =
    match val1, val2 with
    | EvalInt i1, EvalInt i2 -> EvalReturn (env, EvalInt (i1 + i2))
    | EvalInt i1, EvalRatio (i2, i3) ->
        begin match eval_ratio env ((i1 * i3) + i2) i3 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalInt i3 ->
        begin match eval_ratio env (i1 + (i2 * i3)) i2 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalRatio (i3, i4) ->
        begin match eval_ratio env ((i1 * i4) + (i2 * i3)) (i2 * i4) with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | _ -> raise (EvalError "ADDITION-error")
  in
  if 0 = cons_length expr then
    raise (EvalError "ADDITION-error")
  else
    begin match expr with
    | Node (a, Leaf _) ->
      begin match eval_expr env a with
      | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
      end
    | Node (a, Node b) ->
        let eval_ret1 = eval_expr env a in
        let eval_ret2 = lisp_add (Env.update env (get_env eval_ret1)) (Node b) in
        result (Env.update env (get_env eval_ret2)) (get_eval eval_ret1) (get_eval eval_ret2)
    | _ -> raise (EvalError "ADDITION-error")
    end
and lisp_sub env expr =
  let result env val1 val2 =
    match val1, val2 with
    | EvalInt i1, EvalInt i2 -> EvalReturn (env, EvalInt (i1 - i2))
    | EvalInt i1, EvalRatio (i2, i3) ->
        begin match eval_ratio env ((i1 * i3) - i2) i3 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalInt i3 ->
        begin match eval_ratio env (i1 - (i2 * i3)) i2 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalRatio (i3, i4) ->
        begin match eval_ratio env ((i1 * i4) - (i2 * i3)) (i2 * i4) with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | _ -> raise (EvalError "SUBTRACTION-error")
  in
  let rec sub env expr acc =
    match expr with
    | Node (a, Leaf _) ->
        begin match acc with
        | Some EvalReturn (_, ev) ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            Some (result env2 ev (get_eval eval_ret))
        | None ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            Some (EvalReturn (env2, (get_eval eval_ret)))
        end
    | Node (a, Node b) ->
        begin match acc with
        | Some EvalReturn (_, ev) ->
            let eval_ret1 = eval_expr env a in
            let eval_ret2 = result (Env.update env (get_env eval_ret1)) ev (get_eval eval_ret1) in
            sub (get_env eval_ret2) (Node b) (Some eval_ret2)
        | None ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            sub env2 (Node b) (Some (EvalReturn (env2, (get_eval eval_ret))))
        end
    | _ -> raise (EvalError "SUBTRACTION-error")
  in
  if 0 = cons_length expr then
    raise (EvalError "SUBTRACTION-error")
  else if 1 = cons_length expr then
    begin match eval_expr env expr with
    | EvalReturn (env2, EvalInt i) -> EvalReturn ((Env.update env env2), EvalInt (-i))
    | EvalReturn (env2, EvalRatio (i1, i2)) -> EvalReturn ((Env.update env env2), EvalRatio ((-i2), i1))
    | _ -> raise (EvalError "SUBTRACTION-error")
    end
  else
    begin match sub env expr None with
    | Some EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
    | _ -> raise (EvalError "SUBTRACTION-error")
    end
and lisp_mult env expr =
  let result env val1 val2 =
    match val1, val2 with
    | EvalInt i1, EvalInt i2 -> EvalReturn (env, EvalInt (i1 * i2))
    | EvalInt i1, EvalRatio (i2, i3) ->
        begin match eval_ratio env (i1 * i2) i3 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalInt i3 ->
        begin match eval_ratio env (i1 * i3) i2 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalRatio (i3, i4) ->
        begin match eval_ratio env (i1 * i3) (i2 * i4) with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | _ -> raise (EvalError "MULTIPLICATION-error")
  in
  if 0 = cons_length expr then
    raise (EvalError "MULTIPLICATION-error")
  else
    begin match expr with
    | Node (a, Leaf _) ->
        begin match eval_expr env a with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | Node (a, Node b) ->
        let eval_ret1 = eval_expr env a in
        let eval_ret2 = lisp_mult (Env.update env (get_env eval_ret1)) (Node b) in
        result (Env.update env (get_env eval_ret2)) (get_eval eval_ret1) (get_eval eval_ret2)
    | _ -> raise (EvalError "MULTIPLICATION-error")
    end
and lisp_div env expr =
  let result env val1 val2 =
    match val1, val2 with
    | EvalInt i1, EvalInt i2 ->
        begin match eval_ratio env i1 i2 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalInt i1, EvalRatio (i2, i3) ->
        begin match eval_ratio env (i1 * i3) i2 with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalInt i3 ->
        begin match eval_ratio env i1 (i2 * i3) with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | EvalRatio (i1, i2), EvalRatio (i3, i4) ->
        begin match eval_ratio env (i1 * i4) (i2 * i3) with
        | EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
        end
    | _ -> raise (EvalError "DIVISION-error")
  in
  let rec div env expr acc =
    match expr with
    | Node (a, Leaf _) ->
        begin match acc with
        | Some EvalReturn (_, ev) ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            Some (result env2 ev (get_eval eval_ret))
        | None ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            Some (EvalReturn (env2, (get_eval eval_ret)))
        end
    | Node (a, Node b) ->
        begin match acc with
        | Some EvalReturn (_, ev) ->
            let eval_ret1 = eval_expr env a in
            let eval_ret2 = result (Env.update env (get_env eval_ret1)) ev (get_eval eval_ret1) in
            div (get_env eval_ret2) (Node b) (Some eval_ret2)
        | None ->
            let eval_ret = eval_expr env a in
            let env2 = Env.update env (get_env eval_ret) in
            div env2 (Node b) (Some (EvalReturn (env2, (get_eval eval_ret))))
        end
    | _ -> raise (EvalError "DIVISION-error")
  in
  if 0 = cons_length expr then
    raise (EvalError "DIVISION-error")
  else if 1 = cons_length expr then
    begin match eval_expr env expr with
    | EvalReturn (env2, EvalInt i) ->
        begin match eval_ratio (Env.update env env2) 1 i with
        | EvalReturn (env3, ev) -> EvalReturn ((Env.update env env3), ev)
        end
    | EvalReturn (env2, EvalRatio (i1, i2)) ->
        begin match eval_ratio (Env.update env env2) i2 i1 with
        | EvalReturn (env3, ev) -> EvalReturn ((Env.update env env3), ev)
        end
    | _ -> raise (EvalError "DIVISION-error")
    end
  else
    begin match div env expr None with
    | Some EvalReturn (env2, ev) -> EvalReturn ((Env.update env env2), ev)
    | _ -> raise (EvalError "DIVISION-error")
    end
and eval_fun env expr =
  match expr with
  | Node (e1, e2) ->
    begin match e1 with
    | Leaf Symbol "PRINT" ->   lisp_print env e2
    | Leaf Symbol "SETQ" ->    lisp_setq env e2
    | Leaf Symbol "DOTIMES" -> lisp_dotimes env e2
    | Leaf Symbol "+" ->       lisp_add env e2
    | Leaf Symbol "-" ->       lisp_sub env e2
    | Leaf Symbol "*" ->       lisp_mult env e2
    | Leaf Symbol "/" ->       lisp_div env e2
    | _ -> raise (EvalError "unrecognizable-fun-eval-error")
    end
  | _ -> raise (EvalError "fun-eval-error")
and eval_ratio env i1 i2 =
  if 0 = i2 then
    raise (EvalError "division-by-zero-eval-error")
  else
    begin
    let d = gcd i1 i2 in
    if 1 = i2 / d then
      EvalReturn (env, EvalInt (i1 / d))
    else
      begin
      if (i2 / d) < 0 then
        EvalReturn (env, EvalRatio ((-(i1 / d)), (-(i2 / d))))
      else
        EvalReturn (env, EvalRatio ((i1 / d), (i2 / d)))
      end
    end
and eval_number env = function
  | Integer i -> EvalReturn (env, EvalInt i)
  | Ratio (i1, i2) ->
      begin match eval_ratio env i1 i2 with
      | EvalReturn (env2, r) -> EvalReturn ((Env.update env env2), r)
      end
and eval_symbol env str =
  try
    EvalReturn (env, (Env.get_local_value env str))
  with
    EvalError _ -> EvalReturn (env, (Env.get_global_value env str))
and eval_expr env expr =
  match expr with
  | Node _ ->
      begin match eval_fun env expr with
      | EvalReturn (env2, f) -> EvalReturn ((Env.update env env2), f)
      end
  | Leaf Symbol sym ->
      begin match eval_symbol env sym with
      | EvalReturn (env2, s) -> EvalReturn ((Env.update env env2), s)
      end
  | Leaf Number num ->
      begin match eval_number env num with
      | EvalReturn (env2, n) -> EvalReturn ((Env.update env env2), n)
      end
  | Leaf (String str) -> EvalReturn (env, EvalString str)
  | Leaf (Char c) -> EvalReturn (env, EvalChar c)
  | Leaf Nil -> EvalReturn (env, EvalNil)
and eval env expr =
  match expr with
  | Node (e1, e2) ->
      begin match eval_expr env e1 with
      | EvalReturn (env2, _) -> (); eval (Env.update env env2) e2
      end
  | Leaf _ -> eval_expr env expr

let run_program expr =
  match eval Env.empty expr with
  | _ -> ()