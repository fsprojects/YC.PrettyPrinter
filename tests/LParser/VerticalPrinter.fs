module VerticalPrinter

open Expr
open Stmt
open System

let rec exprPrinter (e : Expr.t) : string =
  match e with
  | Num x -> x.ToString () + "\n"
  | Var x -> x + "\n"
  | BinOp (op, x, y) ->
    String.Format("{0}\n{1}{2}", op.ToString (),
                  exprPrinter x, exprPrinter y)

let rec printer (s : Stmt.t) =
  match s with
  | Read  x -> String.Format("read\n{0}\n", x)
  | Write e -> String.Format("write\n{0}", exprPrinter e)
  | Assign (x, e)  -> String.Format(":=\n{0}\n{1}", x, exprPrinter e)
  | Seq  (s1, s2)  -> String.Format(";\n{0}{1}", printer s1, printer s2)
  | If (e, s1, s2) ->
    String.Format("if\n{0}{1}{2}", exprPrinter e,
                  printer s1, printer s2)
  | While (e, s) ->
    String.Format("while\n{0}{1}", exprPrinter e, printer s)


let f x = 
   if x then failwith "error in true branch"
   else failwith "error in false branch"