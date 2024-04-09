open Syntax

let evalto = " evalto "
let indent i = String.make i ' '

type var_rule = EVar1 | EVar2

type op_rule =
  | EPlus
  | EMinus
  | ETimes
  | ELt
  | EPlusBoolL
  | EPlusBoolR
  | EPlusErrorL
  | EPlusErrorR
  | EMinusBoolL
  | EMinusBoolR
  | EMinusErrorL
  | EMinusErrorR
  | ETimesBoolL
  | ETimesBoolR
  | ETimesErrorL
  | ETimesErrorR
  | ELtBoolL
  | ELtBoolR
  | ELtErrorL
  | ELtErrorR

type if_rule = EIfT | EIfF | EIfInt | EIfError | EIfTError | EIfFError
type let_rule = ELetError1 | ELetError2 | ELet

let bplus v1 v2 v i =
  indent i ^ string_of_value v1 ^ " plus " ^ string_of_value v2 ^ " is "
  ^ string_of_value v ^ " by B-Plus {};\n"

let bminus v1 v2 v i =
  indent i ^ string_of_value v1 ^ " minus " ^ string_of_value v2 ^ " is "
  ^ string_of_value v ^ " by B-Minus{};\n"

let btimes v1 v2 v i =
  indent i ^ string_of_value v1 ^ " times " ^ string_of_value v2 ^ " is "
  ^ string_of_value v ^ " by B-Times{};\n"

let bless v1 v2 v i =
  indent i ^ string_of_value v1 ^ " less than " ^ string_of_value v2 ^ " is "
  ^ string_of_value v ^ " by B-Lt{};\n"

exception NotEvaluated

let infer_var env x =
  match env with
  | Env.Emp -> raise Not_found
  | Env.Env (new_env, y, v) -> if x = y then EVar1 else EVar2

let infer_op op v1 v2 =
  match op with
  | Plus -> (
      match (v1, v2) with
      | Value.Bool _, _ -> EPlusBoolL
      | _, Value.Bool _ -> EPlusBoolR
      | Value.Error, _ -> EPlusErrorL
      | _, Value.Error -> EPlusErrorR
      | Value.Num _, Value.Num _ -> EPlus
      | _ -> raise NotEvaluated)
  | Minus -> (
      match (v1, v2) with
      | Value.Bool _, _ -> EMinusBoolL
      | _, Value.Bool _ -> EMinusBoolR
      | Value.Error, _ -> EMinusErrorL
      | _, Value.Error -> EMinusErrorR
      | Value.Num _, Value.Num _ -> EMinus
      | _ -> raise NotEvaluated)
  | Times -> (
      match (v1, v2) with
      | Value.Bool _, _ -> ETimesBoolL
      | _, Value.Bool _ -> ETimesBoolR
      | Value.Error, _ -> ETimesErrorL
      | _, Value.Error -> ETimesErrorR
      | Value.Num _, Value.Num _ -> ETimes
      | _ -> raise NotEvaluated)
  | Less -> (
      match (v1, v2) with
      | Value.Bool _, _ -> ELtBoolL
      | _, Value.Bool _ -> ELtBoolR
      | Value.Error, _ -> ELtErrorL
      | _, Value.Error -> ELtErrorR
      | Value.Num _, Value.Num _ -> ELt
      | _ -> raise NotEvaluated)

let infer_if v1 v2 v3 =
  match (v1, v2, v3) with
  | Value.Num _, _, _ -> EIfInt
  | Value.Error, _, _ -> EIfError
  | Value.Bool true, Value.Error, _ -> EIfTError
  | Value.Bool false, _, Value.Error -> EIfFError
  | Value.Bool true, v, _ -> EIfT
  | Value.Bool false, _, v -> EIfF
  | Value.None, _, _ -> raise NotEvaluated

let infer_let v1 v2 =
  match (v1, v2) with
  | Value.Error, _ -> ELetError1
  | _, Value.Error -> ELetError2
  | _ -> ELet

let rec g_expr env expr i =
  match expr with
  | Num n ->
      indent i ^ string_of_env env ^ " |- " ^ string_of_int n ^ evalto
      ^ string_of_int n ^ " by E-Int {};\n"
  | Bool b ->
      indent i ^ string_of_env env ^ " |- " ^ string_of_bool b ^ evalto
      ^ string_of_bool b ^ " by E-Bool {};\n"
  | Variable (x, v) -> (
      let rule = infer_var env x in
      indent i
      ^ string_of_judg (Evalto (env, expr, v))
      ^
      match rule with
      | EVar1 -> " by E-Var1 {};\n"
      | EVar2 ->
          let new_env = Env.pop env in
          " by E-Var2 {\n" ^ g_expr new_env expr (i + 2) ^ indent i ^ "};\n")
  | Op (e1, op, e2, v) ->
      let v1 = get_value e1 in
      let v2 = get_value e2 in
      let rule = infer_op op v1 v2 in
      indent i
      ^ string_of_judg (Evalto (env, expr, v))
      ^ (match rule with
        | EPlus ->
            " by E-Plus {\n"
            ^ g_expr env e1 (i + 2)
            ^ g_expr env e2 (i + 2)
            ^ bplus v1 v2 v (i + 2)
        | EMinus ->
            " by E-Minus {\n"
            ^ g_expr env e1 (i + 2)
            ^ g_expr env e2 (i + 2)
            ^ bminus v1 v2 v (i + 2)
        | ETimes ->
            " by E-Times {\n"
            ^ g_expr env e1 (i + 2)
            ^ g_expr env e2 (i + 2)
            ^ btimes v1 v2 v (i + 2)
        | ELt ->
            " by E-Lt {\n"
            ^ g_expr env e1 (i + 2)
            ^ g_expr env e2 (i + 2)
            ^ bless v1 v2 v (i + 2)
        | EPlusBoolL -> " by E-PlusBoolL {\n" ^ g_expr env e1 (i + 2)
        | EPlusBoolR -> " by E-PlusBoolR {\n" ^ g_expr env e2 (i + 2)
        | EPlusErrorL -> " by E-PlusErrorL {\n" ^ g_expr env e1 (i + 2)
        | EPlusErrorR -> " by E-PlusErrorR {\n" ^ g_expr env e2 (i + 2)
        | EMinusBoolL -> " by E-MinusBoolL {\n" ^ g_expr env e1 (i + 2)
        | EMinusBoolR -> " by E-MinusBoolR {\n" ^ g_expr env e2 (i + 2)
        | EMinusErrorL -> " by E-MinusErrorL {\n" ^ g_expr env e1 (i + 2)
        | EMinusErrorR -> " by E-MinusErrorR {\n" ^ g_expr env e2 (i + 2)
        | ETimesBoolL -> " by E-TimesBoolL {\n" ^ g_expr env e1 (i + 2)
        | ETimesBoolR -> " by E-TimesBoolR {\n" ^ g_expr env e2 (i + 2)
        | ETimesErrorL -> " by E-TimesErrorL {\n" ^ g_expr env e1 (i + 2)
        | ETimesErrorR -> " by E-TimesErrorR {\n" ^ g_expr env e2 (i + 2)
        | ELtBoolL -> " by E-LtBoolL {\n" ^ g_expr env e1 (i + 2)
        | ELtBoolR -> " by E-LtBoolR {\n" ^ g_expr env e2 (i + 2)
        | ELtErrorL -> " by E-LtErrorL {\n" ^ g_expr env e1 (i + 2)
        | ELtErrorR -> " by E-LtErrorR {\n" ^ g_expr env e2 (i + 2))
      ^ indent i ^ "};\n"
  | If (e1, e2, e3, v) ->
      let v1 = get_value e1 in
      let v2 = get_value e2 in
      let v3 = get_value e3 in
      let rule = infer_if v1 v2 v3 in
      indent i
      ^ string_of_judg (Evalto (env, expr, v))
      ^ (match rule with
        | EIfT -> " by E-IfT {\n" ^ g_expr env e1 (i + 2) ^ g_expr env e2 (i + 2)
        | EIfF -> " by E-IfF {\n" ^ g_expr env e1 (i + 2) ^ g_expr env e3 (i + 2)
        | EIfInt -> " by E-IfInt {\n" ^ g_expr env e1 (i + 2)
        | EIfError -> " by E-IfError {\n" ^ g_expr env e1 (i + 2)
        | EIfTError ->
            " by E-IfTError {\n" ^ g_expr env e1 (i + 2) ^ g_expr env e2 (i + 2)
        | EIfFError ->
            " by E-IfFError {\n" ^ g_expr env e1 (i + 2) ^ g_expr env e3 (i + 2))
      ^ indent i ^ "};\n"
  | Let (x, e1, e2, v) ->
      let v1 = get_value e1 in
      let v2 = get_value e2 in
      let new_env = Env.add env x v1 in
      let rule = infer_let v1 v2 in
      indent i
      ^ string_of_judg (Evalto (env, expr, v))
      ^ (match rule with
        | ELet ->
            " by E-Let {\n" ^ g_expr env e1 (i + 2) ^ g_expr new_env e2 (i + 2)
        | ELetError1 -> " by E-LetError1 {\n" ^ g_expr env e1 (i + 2)
        | ELetError2 ->
            " by E-Let {\n" ^ g_expr env e1 (i + 2) ^ g_expr new_env e2 (i + 2))
      ^ indent i ^ "};\n"

(* interpreter *)
let rec g judg = match judg with Evalto (env, expr, v) -> g_expr env expr 0

(* start point *)
(* Eval.f: Syntax.t -> string *)
let rec f judg = g judg
