module Example

open Expr
open Stmt
open Stmt.Parser
open CoreParser

let ast = 
    //(&"if (1+4+5) { write(1+2+3+4+5+6+7+8+9+10+11)} else{ write(1+2+3+4+5+6+7+8+9+10+11)};")
    //(&"if (1+4+5+12+13+13+14+15) { write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)};")
    //(&"if (1+4+5+12+13+13+14+15) {if (1+4+5+12+13+13+14+15) { write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)}; write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)};")
    //(&"if (1+4+5+12+verylongvarname+13+14+15) {if (1+4+5+12+13+13+14+15) { write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)}; write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)};")
    //(&"if (1+4+5+12+verylongvarname+13+14+15) {if (1+4+5+12+13+13+verylongvarname+14+15) { write(1+2+3+4+5+6+7+8+9+10+11+verylongvarname+12+13+13+14+15)} else{ write(1+verylongvarname+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)}; write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)} else{ write(1+2+3+4+5+6+7+8+9+10+11+12+13+13+14+15)};")
    //(&"if (1+4+5+1+11+12+23+34) { write(1+2+3+4+5+6+7+8+9+10+11+234+34343+445645)} else{ write(1+2+3+4+5+6+7+8+9+10+11+234+34343+445645)};")
    //(&"if (1+5+7) { write(1+2)} else{ write(1+222+3+4)};")
    (&"if (1+5+7) { write(1+2)} else{ write(veryVeryVeryVeryLongVarName+222+3+4)};")
    |> Stmt.Parser.parse () 

let width = 25

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
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s) ^^ rightL ";"
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" --- printS t
            let _f = wordL "else" --- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 
    
    let str ast = 
        printS ast
        |> Display.layout_to_string {FormatOptions.Default with PrintWidth = width}


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
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s) >|< wordL ";"
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
//            let _t = wordL "then" --- printS t
//            let _f = wordL "else" --- printS f
//            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 
            let _t = printS t
            let _f = printS f            
            let if1 = (wordL "if" ^^ bracketL(print c)) @@ (wordL "then" ++ _t) @@ (wordL "else" ++ _f )
            let if2 = (wordL "if" ^^ bracketL(print c)) @@ (wordL "then" @@-- _t) @@ (wordL "else" @@-- _f )
            if1 >//< if2
    
    let str ast = 
        printS ast
        |> Pretty.print width

do printfn "%s" <| (SF.str <| fst ast.Head)

do printfn "%s" <| (YCPP.str <| fst ast.Head)
