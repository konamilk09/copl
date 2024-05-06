type t = { nu : Value.nu_t; expr : Syntax.t; value : Value.t; rule : rule }

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

let rec string_of_t { nu; expr; value; rule } i =
  indent i
  ^ (if nu = [] then "|- " else Value.string_of_nu nu ^ " |- ")
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

exception Eval of Value.nu_t * Syntax.t

let () =
  Printexc.register_printer (function
    | Eval (nu, exp) ->
        Some
          ("Eval(" ^ Value.string_of_nu nu ^ "," ^ Syntax.string_of_expr exp
         ^ ")")
    | _ -> None)

let rec g nu expr =
  match expr with
  | Syntax.Num n -> { nu; expr; value = Value.Num n; rule = EInt }
  | Syntax.Bool b -> { nu; expr; value = Value.Bool b; rule = EBool }
  | Syntax.Sharp i ->
      let value = Value.get nu i in
      { nu; expr; value; rule = EVar }
  | Syntax.Op (e1, op, e2) ->
      let t1 = g nu e1 in
      let t2 = g nu e2 in
      let value = eval_op t1.value t2.value op in
      let rule = rule_op op t1 t2 in
      { nu; expr; value; rule }
  | Syntax.If (e1, e2, e3) -> (
      let t1 = g nu e1 in
      let i = eval_if t1.value in
      match i with
      | Value.Bool true ->
          let t2 = g nu e2 in
          let value = t2.value in
          { nu; expr; value; rule = EIfT (t1, t2) }
      | Value.Bool false ->
          let t3 = g nu e3 in
          let value = t3.value in
          { nu; expr; value; rule = EIfF (t1, t3) }
      | Value.Error -> { nu; expr; value = Value.Error; rule = EIfError t1 }
      | _ -> { nu; expr; value = Value.Error; rule = EIfError t1 }
      (* eval_if で Value.Bool と Value.Error 以外返してないからここには来ない *))
  | Syntax.Let (e1, e2) ->
      let t1 = g nu e1 in
      let v1 = t1.value in
      let new_nu = Value.add nu v1 in
      let t2 = g new_nu e2 in
      let v2 = t2.value in
      let rule = ELet (t1, t2) in
      { nu; expr; value = v2; rule }
  | Syntax.LetRec (e1, e2) ->
      let new_nu = Value.add nu (Value.FunRec (nu, e1)) in
      let ret = g new_nu e2 in
      let value = ret.value in
      { nu; expr; value; rule = ELetRec ret }
  | Syntax.Fun e ->
      let value = Value.Fun (nu, e) in
      { nu; expr; value; rule = EFun }
  | Syntax.App (e1, e2) -> (
      let t1 = g nu e1 in
      let v1 = t1.value in
      let t2 = g nu e2 in
      let v2 = t2.value in
      match v1 with
      | Value.Fun (nu2, e0) ->
          let new_nu = Value.add nu2 v2 in
          let ret = g new_nu e0 in
          { nu; expr; value = ret.value; rule = EApp (t1, t2, ret) }
      | Value.FunRec (nu2, e0) ->
          let new_nu = Value.add (Value.add nu2 v1) v2 in
          let ret = g new_nu e0 in
          { nu; expr; value = ret.value; rule = EAppRec (t1, t2, ret) }
      | _ -> raise (Eval (nu, expr)))

(* starting point *)
(* 部分式を評価し、規則を推論する *)
(* Eval.f: Syntax.t -> Eval.t *)
let f expr = g [] expr
