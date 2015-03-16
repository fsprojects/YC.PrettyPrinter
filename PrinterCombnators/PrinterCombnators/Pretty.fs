module Pretty

open Doc
open Format
type Tree =
    | Empty
    | Node of string*Tree*Tree

let rec treeToDoc tree = 
    match tree with
    | Empty -> Text("E")
    | Node(s,l,r) ->
        let dl = treeToDoc l
        let dr = treeToDoc r
        let idl = Indent(1, dl)
        let idr = Indent(1, dr)
        Choice(Above(Text(s), Above(idl, idr)),Beside(Text(s),Beside(dl, dr)))

let cross list1 list2 = [for x in list1 do for y in list2 do yield x,y] 

let rec merge l1 l2 =
    match l1,l2 with
        | x,[] -> x
        | [],y -> y
        | x::tx,y::ty ->
            if x <= y then x::merge tx l2
            else y::merge l1 ty

let mergel list1 =
    List.fold (fun acc elem1 -> merge acc elem1) [] list1

let tooWide wid (format1:Format) (format2:Format) =
    max format1.width (format1.widthLast + format2.width) > wid

let rec dropWhile (f:'a-> bool) (list:list<_>)=
    match list with
    |[] -> []
    |h::t -> if not(f h) then h::dropWhile f t else []
        
let rec docToFormat wid doc =
    match doc with
    | Text(str) -> 
        if str.Length <= wid
        then [stringToFormat str]
        else []
    |Indent(ind, indDoc) ->
        let l = docToFormat (wid-ind) indDoc
        let indList = List.map (indentFormat ind) l
        List.filter (fun (form:Format) -> form.width <= wid) indList
    |Above(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2
        let aboveForL l = List.map (fun r -> l >-< r) dlist2
        mergel (List.map aboveForL dlist1)
    |Beside(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2               
        //let besideForL (l:Format) = List.map (fun r -> l >|< r) (List.ofSeq (Seq.skipWhile (fun (r:Format) -> tooWide wid l r) dlist2))
        let besideForL (l:Format) = List.map (fun r -> l >|< r) (dropWhile (fun (r:Format) -> tooWide wid l r) dlist2)
        mergel (List.map besideForL dlist1)
    |Choice(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2
        merge dlist1 dlist2
let pretty wid doc =
    let list1 = docToFormat (wid) doc
    list1.Head.txtstr 0 ""