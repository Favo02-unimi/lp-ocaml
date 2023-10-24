#load "str.cma"

(* interface of a purely functional stack *)
module type FunctionalStackADT =
  sig
    type 'a stack
    val create : unit -> 'a stack
    val pop : 'a stack -> 'a * 'a stack
    val push : 'a stack -> 'a -> 'a stack
  end;;

(* implementation of a purely functional stack *)
module FunctionalStack : FunctionalStackADT =
  struct
    exception StackIsEmpty

    type 'a stack =
      | Empty
      | List of 'a list

    let create () = Empty

    let pop stack =
      match stack with
        | List (top :: []) -> (top, Empty)
        | List (top :: rem) -> (top, List rem)
        | _ -> raise StackIsEmpty

    let push stack elem =
      match stack with
        | Empty -> List [elem]
        | List list -> List (elem :: list)
  end;;

(* functor to build a polish calculator given an implementation of a functional stack *)
module PolishCalculator (Stack : FunctionalStackADT) :
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
          | [] -> fst (Stack.pop stack)
          | token :: list ->
            match (parse_token token) with
              | Invalid -> (* Invalid = token is a a value, push to stack *)
                  expr_of_parsed_str list (Stack.push stack (Value (int_of_string token)))
              | _ -> (* token is an operator, pop last two expressions and create new expression *)
                  let expr2 = (fst (Stack.pop stack))
                  and op = (parse_token token)
                  and expr1 = (fst (Stack.pop (snd (Stack.pop stack))))
                  in expr_of_parsed_str list (Stack.push (Stack.create ()) (Expr (expr1, op, expr2)))
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

(* use functor to get a polish calculator that works on FunctionalStack *)
module PC = PolishCalculator(FunctionalStack);;

PC.eval (PC.expr_of_string "10 2 + 4 - 3 * 2 / 2 **");;
PC.eval (PC.expr_of_string "25");;
