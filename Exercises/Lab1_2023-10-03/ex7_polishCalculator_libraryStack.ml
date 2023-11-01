#load "str.cma"

(* --- WARNING ---
   This version of PolishCalculator uses ocaml default library stack,
   that is not purely functional (pop, push operations are in-place).
   Check `ex7_polishCalculator.ml for purely functional version
   with custom stack and PolishCalculator functor
   --- WARNING --- *)

module PolishCalculator:
  sig
    type expr
    val expr_of_string : string -> expr
    val eval : expr -> int
  end =
  struct
    (* type expression, its final value (Value) or a subexpression (Expr) *)
    type expr =
      | Expr of expr * sign * expr
      | Value of int
    (* type sign, available operators (and Invalid, invalid operator) *)
    and sign =
      | Plus
      | Minus
      | Mult
      | Div
      | Pow
      | Invalid

    (* given a token (type string), return the correspondig constructor (type sign) *)
    let parse_token token =
      match token with
        | "+" -> Plus
        | "-" -> Minus
        | "*" -> Mult
        | "/" -> Div
        | "**" -> Pow
        | _ -> Invalid

    (* parse str (type string) to an expression (type expr) *)
    let expr_of_string str =
      let rec expr_of_parsed_str str_list stack =
        match str_list with
          | [] -> Stack.pop stack
          | token :: list ->
            match (parse_token token) with
              | Invalid -> (* Invalid = token is a a value, push to stack *)
                  Stack.push (Value (int_of_string token)) stack;
                  expr_of_parsed_str list stack
              | _ -> (* token is an operator, pop last two expressions and create new expression *)
                  (Stack.push (Expr ((Stack.pop stack), (parse_token token), (Stack.pop stack))) stack);
                  expr_of_parsed_str list stack
      (* split incoming string on spaces, initialize empty stack *)
      in expr_of_parsed_str (Str.split (Str.regexp " ") str) (Stack.create ())

    (* parse operator (type sign) into a function that operates on two integers (type fun int int -> int) *)
    let parse_op op =
      (* power function *)
      let pow num exp =
        let rec pow num exp res =
          match exp with
            | 1 -> res
            | _ -> pow num (exp-1) (res*num)
        in pow num exp num
      in match op with
        | Plus -> (+)
        | Minus -> (-)
        | Mult -> ( * )
        | Div -> (/)
        | Pow -> pow
        | _ -> (+)

    (* evaluate expression (type expr) to its final value (type integer) *)
    let eval expr =
      let rec eval expr =
        match expr with
          | Value i -> i
          | Expr (e1, op, e2) -> (parse_op op) (eval e1) (eval e2)
      in eval expr
  end;;

PolishCalculator.eval (PolishCalculator.expr_of_string "10 2 + 4 - 3 * 2 / 2 **");;
PolishCalculator.eval (PolishCalculator.expr_of_string "25");;
