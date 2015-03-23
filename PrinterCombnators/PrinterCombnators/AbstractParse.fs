module AbstractParse

open Pretty
open Format
open Doc

//type Code = 
//    | Empty
//    | BExpr of string
//    | Expr of string
//    | IFexp of Code
//    | WhExp of Code
//    | PrintExp of string
//
//let rec codeToDoc list =
//    match list with
//    |Empty -> Text("")
//    |BExpr(str) -> Text(str)
//    |Expr(str) -> Text(str)
//    |IFexp(c1) -> 
//        let cD = codeToDoc c1
//        Beside(Text"If ", cD) >//< Above(Text"If ", Indent(1, cD)) >//< Fill(Text"If ", 1, cD)
//    | WhExp(c1) ->  
//        let cD = codeToDoc c1
//        Beside((Text"while ", cD)) >//< Above(Text"while ", Indent(1, cD)) >//< Fill(Text"If ", 1, cD)
//    | PrintExp(str) -> Text("print")

type Code = 
    | Bexpr of string
    | Expr of string
    | IFexp of list<Code>
    | Whexp of list<Code>
    | PrintExp of string
    | WriteExp

let rec listToDoc list =
    match list with
    |[]-> Text("")
    |h::t ->
        match h with
        |Bexpr(str) -> Beside(Text(str + " dob "), listToDoc t) >//< Above(Text(str + " doa "), Indent(1, listToDoc t)) >//< Fill(Text(str + " dof "), 1, listToDoc t)
        //Beside(Text(str + " do"), listToDoc t) >//< Above(Text(str + " do"), Indent(1, listToDoc t)) >//< Fill(Text(str + " do")), 1, listToDoc t)
        |WriteExp -> Above(Text("write sum"), listToDoc t)
        |Expr(str) -> if t.IsEmpty then Text(str) else Above(Text(str), listToDoc t)
        |IFexp(l) ->  Above (Beside(Text("if"), listToDoc l) >//< Fill(Text("if"), 1, listToDoc l), listToDoc t)
        |Whexp(l) ->  Above (Beside(Text("while"), listToDoc l) >//< Fill(Text("while"), 1, listToDoc l), listToDoc t)
        | PrintExp(str) ->  Above(Text("print " + str), listToDoc t)

