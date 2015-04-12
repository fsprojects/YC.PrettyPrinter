//module Test
//
//open Format
//open Doc
//open Pretty
//open AbstractParse
////
////let tree = Node("5", Node("6", Empty, Empty), Empty)
////let doc = treeToDoc tree
////let fmt = docToFormats 25 doc
////stringToFormat("if") >|<
////let fmt2 = stringToFormat("if") >|< stringToFormat(" expr ") >/< (stringToFormat("[") >-< indentFormat 1 (stringToFormat("***") >-< stringToFormat("***")) >-< stringToFormat("]"))
////printfn "%A " (fmt2.widthFirst)
////printfn "%A " (fmt2.width)
////printfn "%A " (fmt2.widthLast)
////printfn "%A " (fmt2.txtstr 0 "")
////printfn "||||||||||||||||"
//
//
////let p  = stringToFormat ("rerererererere \n rerere")
////let p2  = stringToFormat ("qqqqqqqqqqqq \n qqqq")
////let p3  = stringToFormat ("ccccccccccc \n cccccc")
////let f = Format.addFill(p,p2,100)
////printfn "%A" (f.txtstr 5 "")
////printfn "%A first" (f.widthFirst)
////printfn "%A mid" (f.widthLast)
////printfn "%A last" (f.widthLast)
//
//
////printfn "%A" (fmt)
//
//let prints (list : Map<Frame, Format>) = 
//    for x in list do
//        printfn "----------"
//       // printfn "%A width" (x.Key.width)
//       // printfn "%A last" (x.Key.widthLast)
//        printfn "%A First" (x.Value.widthFirst)
//        printfn "%A width1" (x.Value.width)
//        printfn "%A last1" (x.Value.widthLast)
//        printfn "%A" (x.Value.txtstr 0 "")
//let l = [IFexp[Bexpr(" x>0 "); Expr("{"); PrintExp("thisPrint"); Expr("***");Expr("}")]; PrintExp("second "); Whexp[Bexpr(" x<99 "); Expr("{"); Expr("****");Expr("}")];]
////prints (docToFormats  30 (listToDoc l))
////let p = "sdsad \n fdsfsdf     \r\nasfsf"
////let k = pretty 30 (listToDoc l)
////System.IO.File.WriteAllText("d:\\a.txt", k.txtstr 0 "")
////System.IO.File.WriteAllText("d:\\a.txt", p)
////printfn "%A" (prints fmt)001
//let n = Text "First" >-< Text "fdsfdsfsdfdsfdsfsddsf" |> pretty 10//docToFormats 10
//printfn "asdasd" 
//n.txtstr 0 ""
//
//
//
////let a = "a"
////let b = "b"
//////let (p:Format) = null
////let dlist2 = Map.empty//.Add(new Frame(b.Length,b.Length,b.Length), stringToFormat b)
////let aboveL (l:Format) = Map.fold (fun acc key (r:Format) -> update (l >-< r) acc) Map.empty dlist2
////let dlist1 = Map.empty.Add(new Frame(a.Length, a.Length, a.Length), stringToFormat a)
////let res = aboveL (stringToFormat a)
////printfn "%A" (res.[new Frame(b.Length,b.Length,b.Length)].txtstr 0 "")
////Спросить у gsv