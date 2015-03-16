module Test 

open Format
open Doc
open Pretty

//let mydoc = Choice(Above(Text("5"),Indent(1, Text("6"))), Beside(Text("5"), Indent(1, Text("6"))))
//let fmts = docToFormat 10 mydoc
//printfn "%A" (fmts)
//let fmts = docToFormat 10 (Beside(Text("5"),Text("6")))
//printfn "%A" (fmts.Head.txtstr 0 "")
//printfn "%A" "----------------"
//printfn "%A" (fmts.Tail.Head.txtstr 0 "")
//printfn "%A" "----------------"

//let p  = stringToFormat ("rerererererere \n rerere")
//let p2  = stringToFormat ("qqqqqqqqqqqq \n qqqq")
//let p3  = stringToFormat ("ccccccccccc \n cccccc")
//let f = p >|< p2
//printfn "%A" (f.txtstr 5 "")
//printfn "%A" (spaces 5)
//printfn "%A" (f.txtstr 0 "")

let tree = Node("5",Node("6",Empty,Empty), Empty)
let doc = treeToDoc tree
//let pr = pretty 25 doc
let fmt = docToFormat 5 doc
//printfn "%A" (mergel [[1] ;[0] ; [4] ;[10]])
printfn "%A" (pretty 5 doc)
//printfn "%A" (doc)
printfn "%A" (fmt)
printfn "%A" (fmt.Head.txtstr 0 "")
printfn "%A" "----------------"
printfn "%A" (fmt.Tail.Head.txtstr 0 "")
printfn "%A" "----------------"
printfn "%A" (fmt.Tail.Tail.Head.txtstr 0 "")
printfn "%A" "----------------"
printfn "%A" (fmt.Tail.Tail.Tail.Head.txtstr 0 "")
printfn "%A" "----------------"
//printfn "%A" (fmt.Head.width)
//printfn "%A" (fmt.Tail.Head.width)
//printfn "%A" (fmt.Tail.Tail.Head.width)
//printfn "%A" (fmt.Tail.Tail.Tail.Head.width)


//let addOne x = x + 1
//let timesTwo x = 2 * x
//let Compose1 = addOne << timesTwo

// Result is 5
//let result1 = Compose1 2