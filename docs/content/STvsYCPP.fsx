(**
Why YC.PrettyPrinter is not yiet another Microsoft.Text.StructuredFormat? Lets try to compare this two libraries. Final version of sources presented in this post you can found on <a href="https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/tests/Example/Program.fs">GitHub</a>.

Documentation you can found here and here

First of all, common header. I use custom parser for simple "C-like" language implemented for tests. In examples below I do not aimed to format input string. So, input and output syntax may be different. Also let we specify width for result layout.

*)

module Example

open Expr
open Stmt
open Stmt.Parser
open CoreParser

let ast = 
    &"if (1+5+7) { write(1+2); write(1+2)} else{ write(veryVeryVeryVeryLongVarName+222+3+4)};"
    |> Stmt.Parser.parse () 
    |> List.head
    |> fst

let width = 25

(**

Printer with braking after "then" and "else".

*)

module SF =
    open Microsoft.FSharp.Text.StructuredFormat
    open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

    let rec print ast =
        match ast with
        | Expr.BinOp(x, y, z) -> print y ++ wordL (string x) ^^ print z
        | Expr.Num n -> string n |> wordL
        | Expr.Var n -> string n |> wordL
    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" @@-- printS t
            let _f = wordL "else" @@-- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 
    
    let str ast = 
        printS ast
        |> Display.layout_to_string {FormatOptions.Default with PrintWidth = width}

(**

Output

*)



(**

Copy to YCPP

*)


module YCPP =
    open YC.PrettyPrinter
    open YC.PrettyPrinter.StructuredFormat

    let rec print ast =
        match ast with
        | Expr.BinOp(x, y, z) -> print y ++ wordL (string x) ^^ print z
        | Expr.Num n -> string n |> wordL
        | Expr.Var n -> string n |> wordL
    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" @@-- printS t
            let _f = wordL "else" @@-- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 

    let str ast = 
        printS ast
        |> Pretty.print width

(**

Good. Similar result but som problems with semicolon. Yes, rightL is stub for migration simplification. So, let try ti fix it: wordL and unbracable concatenation without spasec can help to solve problem. 

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2

(**

Well. Migration is pretty simple. But what about differences? Let try to print variadic "if".

*)


    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" --- printS t
            let _f = wordL "else" --- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 


(**

Some examples are good but not all. And we can not solve this problem in SF. But YCPP can help us.

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = printS t
            let _f = printS f            
            let ifTpl f = (wordL "if" ^^ bracketL(print c)) @@ (wordL "then" |> f <| _t) @@ (wordL "else" |> f <| _f )            
            (ifTpl (^^)) >//< (ifTpl (@@--))

(**

Well. All works fine.

*)