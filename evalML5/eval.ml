type t = { env : Value.env_t; expr : Syntax.t; value : Value.t; rule : rule }
and match_t = { judgm : Value.judg_t; envm : Value.env_t; rulem : j_mrule }
and nmatch_t = { judgn : Value.judg_t; rulenm : j_nmrule }
and j_mrule = MVar | MNil | MCons of match_t * match_t | MWild

and j_nmrule =
  | NMConsNil
  | NMNilCons
  | NMConsConsL of nmatch_t
  | NMConsConsR of nmatch_t

and rule =
  | EInt
  | EBool
  | EVar
  | EPlus of t * t
  | EMinus of t * t
  | ETimes of t * t
  | ELt of t * t
  | BPlus
  | BMinus
  | BTimes
  | BLt
  | EIfT of t * t
  | EIfF of t * t
  | ELet of t * t
  | EFun
  | EApp of t * t * t
  | ELetRec of t
  | EAppRec of t * t * t
  | EIfError of t
  | ENil
  | ECons of t * t
  | EMatchM1 of t * match_t * t
  | EMatchM2 of t * match_t * t
  | EMatchN of t * nmatch_t * t

let indent i = String.make i ' '

(* print *)
let string_of_bplus v1 v2 value i =
  indent i ^ Value.string_of_value v1 ^ " plus " ^ Value.string_of_value v2
  ^ " is "
  ^ Value.string_of_value value
  ^ " by B-Plus {};\n"

let string_of_bminus v1 v2 value i =
  indent i ^ Value.string_of_value v1 ^ " minus " ^ Value.string_of_value v2
  ^ " is "
  ^ Value.string_of_value value
  ^ " by B-Minus {};\n"

let string_of_btimes v1 v2 value i =
  indent i ^ Value.string_of_value v1 ^ " times " ^ Value.string_of_value v2
  ^ " is "
  ^ Value.string_of_value value
  ^ " by B-Times {};\n"

let string_of_blt v1 v2 value i =
  indent i ^ Value.string_of_value v1 ^ " less than " ^ Value.string_of_value v2
  ^ " is "
  ^ Value.string_of_value value
  ^ " by B-Lt {};\n"

let rec string_of_match { judgm; envm; rulem } i =
  indent i ^ Value.string_of_judg judgm ^ " when (" ^ Value.string_of_env envm
  ^ ")"
  ^
  match rulem with
  | MVar -> " by M-Var {};\n"
  | MNil -> " by M-Nil {};\n"
  | MCons (j1, j2) ->
      " by M-Cons {\n"
      ^ string_of_match j1 (i + 2)
      ^ string_of_match j2 (i + 2)
      ^ indent i ^ "};\n"
  | MWild -> " by M-Wild {};\n"

let rec string_of_nmatch { judgn; rulenm } i =
  indent i ^ Value.string_of_judg judgn
  ^
  match rulenm with
  | NMConsNil -> " by NM-ConsNil {};\n"
  | NMNilCons -> " by NM-NilCons {};\n"
  | NMConsConsL j ->
      " by NM-ConsConsL {\n" ^ string_of_nmatch j (i + 2) ^ indent i ^ "};\n"
  | NMConsConsR j ->
      " by NM-ConsConsR {\n" ^ string_of_nmatch j (i + 2) ^ indent i ^ "};\n"

let rec string_of_t { env; expr; value; rule } i =
  indent i
  ^ (if env = [] then "|- " else Value.string_of_env env ^ " |- ")
  ^ Syntax.string_of_expr expr ^ " evalto "
  ^ Value.string_of_value value
  ^
  match rule with
  | EInt -> " by E-Int {};\n"
  | EBool -> " by E-Bool {};\n"
  | EVar -> " by E-Var {};\n"
  | EPlus (t1, t2) ->
      " by E-Plus {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_bplus t1.value t2.value value (i + 2)
      ^ indent i ^ "};\n"
  | EMinus (t1, t2) ->
      " by E-Minus {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_bminus t1.value t2.value value (i + 2)
      ^ indent i ^ "};\n"
  | ETimes (t1, t2) ->
      " by E-Times {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_btimes t1.value t2.value value (i + 2)
      ^ indent i ^ "};\n"
  | ELt (t1, t2) ->
      " by E-Lt {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_blt t1.value t2.value value (i + 2)
      ^ indent i ^ "};\n"
  | EIfT (t1, t2) ->
      " by E-IfT {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | EIfF (t1, t2) ->
      " by E-IfF {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | ELet (t1, t2) ->
      " by E-Let {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | EFun -> " by E-Fun {};\n"
  | ELetRec t -> " by E-LetRec {\n" ^ string_of_t t (i + 2) ^ indent i ^ "};\n"
  | EApp (t1, t2, t3) ->
      " by E-App {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_t t3 (i + 2)
      ^ indent i ^ "};\n"
  | EAppRec (t1, t2, t3) ->
      " by E-AppRec {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ string_of_t t3 (i + 2)
      ^ indent i ^ "};\n"
  | ENil -> " by E-Nil {};\n"
  | ECons (t1, t2) ->
      " by E-Cons {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | EMatchM1 (t1, j, t2) ->
      " by E-MatchM1 {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_match j (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | EMatchM2 (t1, j, t2) ->
      " by E-MatchM2 {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_match j (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | EMatchN (t1, j, t2) ->
      " by E-MatchN {\n"
      ^ string_of_t t1 (i + 2)
      ^ string_of_nmatch j (i + 2)
      ^ string_of_t t2 (i + 2)
      ^ indent i ^ "};\n"
  | _ -> ""

let print expr =
  let str = string_of_t expr 0 in
  print_string str

exception NotSupported of string

let eval_op v1 v2 op =
  match (v1, v2) with
  | Value.Num i1, Value.Num i2 -> (
      match op with
      | Syntax.Plus -> Value.Num (i1 + i2)
      | Syntax.Minus -> Value.Num (i1 - i2)
      | Syntax.Times -> Value.Num (i1 * i2)
      | Syntax.Lt -> Value.Bool (i1 < i2))
  | Value.Bool i1, _ -> Value.Error
  | _, Value.Bool i2 -> Value.Error
  | Value.Error, _ -> Value.Error
  | _, Value.Error -> Value.Error
  | _ -> raise (NotSupported "None")

let rule_op op t1 t2 =
  match op with
  | Syntax.Plus -> EPlus (t1, t2)
  | Syntax.Minus -> EMinus (t1, t2)
  | Syntax.Times -> ETimes (t1, t2)
  | Syntax.Lt -> ELt (t1, t2)

let eval_if v1 =
  match v1 with
  | Value.Bool b -> Value.Bool b
  | Value.Num i -> Value.Error
  | Value.Error -> Value.Error
  | _ -> raise (NotSupported "None")

exception Eval of Value.env_t * Syntax.t * string
exception MatchError of Syntax.pattern_t * Value.t * string

let () =
  Printexc.register_printer (function
    | Eval (env, exp, "") ->
        Some
          ("Eval(" ^ Value.string_of_env env ^ "," ^ Syntax.string_of_expr exp
         ^ ")")
    | _ -> None)

(* パターンと値を受け取って、それらがマッチするかを真偽値で返す*)
let rec if_match p v =
  match (p, v) with
  | Syntax.PatternV x, _ -> true
  | Syntax.PatternEmpty, Value.EmptyList -> true
  | Syntax.Cons (p1, p2), Value.List (v1, v2) ->
      if_match p1 v1 && if_match p2 v2
  | Syntax.WildCard, _ -> true
  | _ -> false

(* マッチ判断の推論 *)
let rec g_match p v =
  match (p, v) with
  | Syntax.PatternV x, _ ->
      let envm = Value.add [] x v in
      { judgm = Value.Matches (p, v); envm; rulem = MVar }
  | Syntax.PatternEmpty, Value.EmptyList ->
      { judgm = Value.Matches (p, v); envm = []; rulem = MNil }
  | Syntax.Cons (p1, p2), Value.List (v1, v2) ->
      let j1 = g_match p1 v1 in
      let j2 = g_match p2 v2 in
      let envm = Value.union j1.envm j2.envm in
      { judgm = Value.Matches (p, v); envm; rulem = MCons (j1, j2) }
  | Syntax.WildCard, _ ->
      { judgm = Value.Matches (p, v); envm = []; rulem = MWild }
  | _ -> raise (MatchError (p, v, "had match assumption but doesn't match"))

(* マッチしない判断の推論。env は常に空リスト *)
let rec g_nmatch p v =
  match (p, v) with
  | Syntax.PatternEmpty, Value.List (v1, v2) ->
      { judgn = Value.NMatch (p, v); rulenm = NMConsNil }
  | Syntax.Cons (p1, p2), Value.EmptyList ->
      { judgn = Value.NMatch (p, v); rulenm = NMNilCons }
  | Syntax.Cons (p1, p2), Value.List (v1, v2) ->
      if if_match p1 v1 then
        let j2 = g_nmatch p2 v2 in
        { judgn = Value.NMatch (p, v); rulenm = NMConsConsR j2 }
      else
        let j1 = g_nmatch p1 v1 in
        { judgn = Value.NMatch (p, v); rulenm = NMConsConsL j1 }
  | _ -> raise (MatchError (p, v, "had doesn't match assumption"))

(* 式の評価の推論 *)
let rec g env expr =
  match expr with
  | Syntax.Num n -> { env; expr; value = Value.Num n; rule = EInt }
  | Syntax.Bool b -> { env; expr; value = Value.Bool b; rule = EBool }
  | Syntax.Variable x ->
      let value = Value.get env x in
      { env; expr; value; rule = EVar }
  | Syntax.Op (e1, op, e2) ->
      let t1 = g env e1 in
      let t2 = g env e2 in
      let value = eval_op t1.value t2.value op in
      let rule = rule_op op t1 t2 in
      { env; expr; value; rule }
  | Syntax.If (e1, e2, e3) -> (
      let t1 = g env e1 in
      let i = eval_if t1.value in
      match i with
      | Value.Bool true ->
          let t2 = g env e2 in
          let value = t2.value in
          { env; expr; value; rule = EIfT (t1, t2) }
      | Value.Bool false ->
          let t3 = g env e3 in
          let value = t3.value in
          { env; expr; value; rule = EIfF (t1, t3) }
      | Value.Error -> { env; expr; value = Value.Error; rule = EIfError t1 }
      | _ -> { env; expr; value = Value.Error; rule = EIfError t1 }
      (* eval_if で Value.Bool と Value.Error 以外返してないからここには来ない *))
  | Syntax.Let (x, e1, e2) ->
      let t1 = g env e1 in
      let v1 = t1.value in
      let new_env = Value.add env x v1 in
      let t2 = g new_env e2 in
      let v2 = t2.value in
      let rule = ELet (t1, t2) in
      { env; expr; value = v2; rule }
  | Syntax.LetRec (x, y, e1, e2) ->
      let new_env = Value.add env x (Value.FunRec (env, x, y, e1)) in
      let ret = g new_env e2 in
      let value = ret.value in
      { env; expr; value; rule = ELetRec ret }
  | Syntax.Fun (x, e) ->
      let value = Value.Fun (env, x, e) in
      { env; expr; value; rule = EFun }
  | Syntax.App (e1, e2) -> (
      let t1 = g env e1 in
      let v1 = t1.value in
      let t2 = g env e2 in
      let v2 = t2.value in
      match v1 with
      | Value.Fun (env2, x, e0) ->
          let new_env = Value.add env2 x v2 in
          let ret = g new_env e0 in
          { env; expr; value = ret.value; rule = EApp (t1, t2, ret) }
      | Value.FunRec (env2, x, y, e0) ->
          let new_env = Value.add (Value.add env2 x v1) y v2 in
          let ret = g new_env e0 in
          { env; expr; value = ret.value; rule = EAppRec (t1, t2, ret) }
      | _ -> raise (Eval (env, expr, "")))
  | Syntax.EmptyList -> { env; expr; value = Value.EmptyList; rule = ENil }
  | Syntax.List (e1, e2) ->
      let t1 = g env e1 in
      let v1 = t1.value in
      let t2 = g env e2 in
      let v2 = t2.value in
      { env; expr; value = Value.List (v1, v2); rule = ECons (t1, t2) }
  | Syntax.Match (e0, c0) -> (
      let t0 = g env e0 in
      let v = t0.value in
      match c0 with
      | Syntax.Clause1 (p, e) ->
          let j = g_match p v in
          let env2 = Value.append env j.envm in
          let t' = g env2 e in
          let v' = t'.value in
          { env; expr; value = v'; rule = EMatchM1 (t0, j, t') }
      | Syntax.Clause2 (p, e, c) ->
          if if_match p v then
            let j = g_match p v in
            let env2 = Value.append env j.envm in
            let t' = g env2 e in
            let v' = t'.value in
            { env; expr; value = v'; rule = EMatchM2 (t0, j, t') }
          else
            let j = g_nmatch p v in
            let t' = g env (Syntax.Match (e0, c)) in
            let v' = t'.value in
            { env; expr; value = v'; rule = EMatchN (t0, j, t') })

(* starting point *)
(* 部分式を評価し、規則を推論する *)
(* Eval.f: Syntax.t -> Eval.t *)
let f expr = g [] expr
