module YCLPrinter
open Expr
open Stmt
open Stmt.Parser
open CoreParser
open Doc
open Pretty

//open Microsoft.FSharp.Text.StructuredFormat
//open Microsoft.FSharp.Text.StructuredFormat.LayoutOps
open YC.PrinterCombinators.StructuredFormat

type combineOpt =
    |A
    |B
    |AB

type Printer(width, iex, wex) =
    let width = width
    let iF = iex
    let wh = wex

    let rec exprPrinter (e : Expr.t) =
      match e with
      | Num x -> wordL (x.ToString())
      | Var x -> wordL x
      | BinOp (op, x, y) ->  bracketL ((exprPrinter x) ^^ wordL (op.ToString()) ^^ (exprPrinter y))

    let rec printer (s : Stmt.t) =
        match s with
        | Read  x -> wordL"read" ^^ bracketL (wordL x) ^^ wordL";"
        | Write e -> wordL"write" ^^ bracketL (exprPrinter e) ^^ wordL";"
        | Assign (x, e)  ->  wordL x ^^ wordL":=" ^^ exprPrinter e ^^ wordL";"
        | Seq  (s1, s2)  -> (printer s1) @@ printer s2
        | If (e, s1, s2) -> ifPrint e s1 s2
        | While (e, s) -> whPrint e s
        
    and ifPrint e s1 s2 = 
        match iF with
        | A -> ((wordL"if" ^^ bracketL (exprPrinter e))  @@--  braceL (printer s1)) @@ wordL"else" @@-- braceL (printer s2)
        | B -> ((wordL"if" ^^ bracketL (exprPrinter e)) ^^ ( braceL (printer s1))) @@ (wordL"else" ^^ braceL (printer s2)) 
        | AB -> ((wordL"if" ^^ bracketL (exprPrinter e)) --- (braceL (printer s1))) @@ (wordL"else" --- braceL (printer s2))
    
    and whPrint e s = 
        match wh with
        |A -> (wordL"while" ^^ bracketL (exprPrinter e)) @@-- braceL (printer s)        
        |B -> wordL"while" ^^ exprPrinter e ^^  braceL (printer s)        
        |AB -> (wordL"while" ^^ bracketL (exprPrinter e)) --- braceL (printer s) 

    member this.Print text =
        let n = parse () &text |> List.head |> fst |> printer 
        //Display.layout_to_string {FormatOptions.Default with PrintWidth=100} n
        n |> (Pretty.prettyPrints width) 

    member this.Print (s : Stmt.t) =
        //s|> printer |> Display.layout_to_string {FormatOptions.Default with PrintWidth=100} 
        s |> printer |> (Pretty.prettyPrints width) 
