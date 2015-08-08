(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Getting started
========================

YC.PrettyPrinters contains two sets of printer combinators: one consists of 5 basic combinators described in the paper
and the second is the same as 
<a href="https://github.com/fsprojects/FSharpx.Extras/tree/master/src/FSharpx.Text.StructuredFormat">FSharp.Text.StructuredFormat</a>
library provides. You can either use the set of basic combinators or the set of more high-level StructuredFormat-like combinators.

The following example demonstrates how to use this library.
*)

#r @"..\..\bin\YC.PrettyPrinter\YC.PrettyPrinter.dll"

open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat

let stmt i = wordL <| "stmt_" + string i

let stmtBlock k =
    [for i in 0 .. k - 1 -> stmt i]
    |> List.reduce (@@)

let layout =
   let condBlock = wordL "if" ^^ bracketL (wordL "cond")
   let thenBlock = wordL "then" -- (stmtBlock 2)
   let elseBlock = wordL "else" -- (stmtBlock 3)
   condBlock
   @@ thenBlock
   @@ elseBlock

/// Method print chooses the best layout for your code.
/// First argument specializes a maximum possible width.
let str = print 10 layout
printfn "Narrow box:\n%s" str

let str2 = print 30 layout
printfn "\nWide box:\n%s" str2

(*
Expected result:

Narrow box:
if (cond)
then
 stmt_0
 stmt_1
else
 stmt_0
 stmt_1
 stmt_2

Wide box:
if (cond)
then stmt_0
     stmt_1
else stmt_0
     stmt_1
     stmt_2
*)


(**
The best way to understand how to create pretty printers using our library is to take a look 
at <a href="https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/src/YC.PrettyPrinter/StructuredFormat.fs">our implementation</a> of
<a href="https://github.com/fsprojects/FSharpx.Extras/tree/master/src/FSharpx.Text.StructuredFormat">FSharp.Text.StructuredFormat</a>. 

*)
