(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let boolToInt v = if v then 1 else 0
    let intToBool v = v != 0
    
    let evalOp operation lhs rhs = match operation with
      | "+" -> lhs + rhs
      | "-" -> lhs - rhs
      | "*" -> lhs * rhs
      | "/" -> lhs / rhs
      | "%" -> lhs mod rhs
      | "<" ->  boolToInt (lhs < rhs)
      | ">" ->  boolToInt (lhs > rhs)
      | "<=" -> boolToInt (lhs <= rhs)
      | ">=" -> boolToInt (lhs >= rhs)
      | "==" -> boolToInt (lhs == rhs)
      | "!=" -> boolToInt (lhs != rhs)
      | "&&" -> boolToInt (intToBool lhs && intToBool rhs)
      | "!!" -> boolToInt (intToBool lhs || intToBool rhs)
      | _ -> failwith(Printf.sprintf "Undefined operator")
    
   let rec eval state expr = match expr with
	  | Const value -> value
	  | Var variable -> state variable
	  | Binop(op, lhs, rhs) -> evalOp op (eval state lhs) (eval state rhs)	


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    let ostapBin op = ostap($(op)), (fun l r -> Binop(op, l, r))
    
    ostap (
        expr:
             !(Ostap.Util.expr
                 (fun x -> x)
                 (Array.map (fun (assoc, ops) -> assoc, List.map ostapBin ops)
                     [|
                             `Lefta, ["!!"];
                             `Lefta, ["&&"];
                             `Nona,  ["<="; "<"; ">="; ">"; "=="; "!="];
                             `Lefta, ["+"; "-"];
                             `Lefta, ["*"; "/"; "%"];
                     |]
                 ) 
                 primary
             );
       primary: n: IDENT {Var n} | v: DECIMAL {Const v} | -"(" expr -")"
    )
  end
    
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | RepeatUntil of t * Expr.t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
 let rec eval conf stmt =
      let (st, i, o) = conf in
      match stmt with
      | Read(var) -> (match i with
        | [] -> failwith (Printf.sprintf "Reached EOF")
        | head :: tail -> (Expr.update var head st, tail, o))
      | Write(expr) -> (st, i, o @ [Expr.eval st expr])
      | Assign(var, expr) -> (Expr.update var (Expr.eval st expr) st, i, o)
      | Seq(stmt1, stmt2) -> eval (eval conf stmt1) stmt2
      | Skip -> conf
      | If(cond, then_branch, else_branch) ->
        if Expr.intToBool (Expr.eval st cond)
        then eval conf then_branch
        else eval conf else_branch
      | While(cond, body) -> 
        let rec evalWhile conf =
          let (st, _, _) = conf in
          if Expr.intToBool (Expr.eval st cond)
          then evalWhile (eval conf body)
          else conf
        in
        evalWhile conf
      | RepeatUntil(body, cond) ->
        let rec evalRepeatUntil conf =
          let conf = eval conf body in
          let (st, _, _) = conf in
          if Expr.intToBool (Expr.eval st cond)
          then conf
          else evalRepeatUntil conf
        in
        evalRepeatUntil conf

    (* Statement parser *)
    ostap (
      parse:
        top_level_stmt;
      top_level_stmt:
        stmt1: stmt -";" stmt2: top_level_stmt { Seq(stmt1, stmt2) } | stmt;
      stmt: 
        read_stmt   |
        write_stmt  |
        assign_stmt |
        skip_stmt   |
        if_stmt     |
        while_stmt  |
        repeat_stmt |
        for_stmt;
      assign_stmt:
        x: IDENT -":=" e: !(Expr.expr) { Assign(x, e) };
      read_stmt:
        - !(Util.keyword)["read"] -"(" x: IDENT -")" { Read(x) };
      write_stmt:
        - !(Util.keyword)["write"] -"(" e: !(Expr.expr) -")" { Write(e) };
      skip_stmt:
        - !(Util.keyword)["skip"] { Skip };
      condition_part:
        cond: !(Expr.expr)
        - !(Util.keyword)["then"] then_branch: top_level_stmt
        { (cond, then_branch) };
      if_stmt:
        - !(Util.keyword)["if"] first_cond: condition_part
        elif_branches: (- !(Util.keyword)["elif"] condition_part)*
        else_branch: (- !(Util.keyword)["else"] top_level_stmt)?
        - !(Util.keyword)["fi"]
        {
          let (cond, body) = first_cond in
          let else_branch = match else_branch with
          | None -> Skip
          | Some stmt -> stmt
          in
          let fold_elif (cond, then_branch) else_branch = 
            If(cond, then_branch, else_branch)
          in
          let elif_bodies =
            List.fold_right fold_elif elif_branches else_branch
          in
          If(cond, body, elif_bodies)
        };
      while_stmt:
        - !(Util.keyword)["while"] cond: !(Expr.expr)
        - !(Util.keyword)["do"]
        body: top_level_stmt
        - !(Util.keyword)["od"]
        { While(cond, body) };
      repeat_stmt:
        - !(Util.keyword)["repeat"]
        body: top_level_stmt
        - !(Util.keyword)["until"]
        cond: !(Expr.expr)
        { RepeatUntil(body, cond) };
      for_stmt:
        - !(Util.keyword)["for"]
        init: stmt -"," cond: !(Expr.expr) -"," inc: stmt
        - !(Util.keyword)["do"] body: top_level_stmt - !(Util.keyword)["od"]
        { Seq(init, While(cond, Seq(body, inc))) }
    )
      
  end
  


(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
