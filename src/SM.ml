open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval_iter (stack, (s, i, o)) program = match program with
                                      | BINOP op -> (match stack with
                                                    | y::x::tail -> ([Language.Expr.evalOp op x y] @ tail, (s, i, o))
                                                    | _ -> conf
                                                    )
                                      | CONST c -> ([c] @ stack, (s, i, o))
                                      | READ -> (match i with
                                            | head :: tail -> ([head] @ stack, (s, tail, o))
                                            | [] -> failwith (Printf.sprintf "EOF is reached")
                                            )
                                      | WRITE -> (match stack with 
                                                | head :: tail -> (tail, (s, i, o @ [head]))
                                                | _ -> conf
                                                 )
                                      | LD x -> ([s x] @ stack, (s, i, o))
                                      | ST x -> (match stack with 
                                                | head::tail -> (tail, (Language.Expr.update x head s, i, o))
                                                | _ -> conf
                                                )


        
let rec eval configuration program = match program with
| [] -> configuration
| instruction :: rest -> eval (eval_iter configuration instruction) rest

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
 
let rec compile statement = 
    let rec compile_expr expr = match expr with
                            | Language.Expr.Const c -> [CONST c]
                            | Language.Expr.Var   n -> [LD n]
                            | Language.Expr.Binop (op, lhs, rhs) -> compile_expr lhs @ compile_expr rhs @ [BINOP op] in match statement with 
                            | Language.Stmt.Read  x -> [READ; ST x]
                            | Language.Stmt.Write expr -> (compile_expr expr) @ [WRITE]
                            | Language.Stmt.Assign  (n, e) -> (compile_expr e) @ [ST n]
| Language.Stmt.Seq (fst, snd) -> (compile fst) @ (compile snd) 
