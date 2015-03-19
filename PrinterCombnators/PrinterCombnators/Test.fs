module Test

open Format
open Doc
open Pretty

let tree = Node("5", Node("6", Empty, Empty), Empty)
let doc = treeToDoc tree
let fmt = docToFormats 3 doc

//printfn "%A" (pretty 5 doc)
//printfn "%A" (doc)
printfn "%A" (fmt)

let prints (list : Map<Frame, Format>) = 
    for x in list do
        printfn "----------"
        printfn "%A width" (x.Key.width)
        printfn "%A last" (x.Key.widthLast)
        printfn "%A width1" (x.Value.width)
        printfn "%A last1" (x.Value.widthLast)
        printfn "%A" (x.Value.txtstr 0 "")

printfn "%A" (prints fmt)