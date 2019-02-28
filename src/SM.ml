open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval_iter (stack, (s, i, o)) program = match program with
                                      | BINOP op -> (match stack with
                                                    | y::x::tail -> ([Syntax.Expr.evalOp op x y] @ tail, (s, i, o))
                                                    | _ -> failwith "Empty stack"
                                                    )
                                      | CONST c -> ([c] @ stack, (s, i, o))
                                      | READ -> (match i with
                                            | head :: tail -> ([head] @ stack, (s, tail, o))
                                            | _ -> failwith "bad istream"
                                            )
                                      | WRITE -> (match stack with 
                                                | head :: tail -> (tail, (s, i, o @ [head]))
                                                | _ -> failwith "Empty stack"
                                                 )
                                      | LD x -> ([s x] @ stack, (s, i, o))
                                      | ST x -> (match stack with 
                                                | head::tail -> (tail, (Syntax.Expr.update x head s, i, o))
                                                | _ -> failwith "Empty stack"
                                                )


                   
                                                        
let eval configuration program = List.fold_left eval_iter configuration program                                     

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile statement = 
    let rec compile_expr expr = match expr with
                            | Syntax.Expr.Const c -> [CONST c]
                            | Syntax.Expr.Var   n -> [LD n]
                            | Syntax.Expr.Binop (op, lhs, rhs) -> compile_expr lhs @ compile_expr rhs @ [BINOP op] in match statement with 
                            | Syntax.Stmt.Read  x -> [READ; ST x]
                            | Syntax.Stmt.Write expr -> (compile_expr expr) @ [WRITE]
                            | Syntax.Stmt.Assign  (n, e) -> (compile_expr e) @ [ST n]
                            | Syntax.Stmt.Seq (fst, snd) ->   (compile fst) @ (compile snd)  




