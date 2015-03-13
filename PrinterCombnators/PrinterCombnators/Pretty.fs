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

let rec docToFormat wid doc =
    match doc with
    | Text(str) -> 
        if str.Length <= wid
        then [stringToFormat str]
        else []
    |Indent(ind, indDoc) ->
        //let l = docToFormat (wid - ind) indDoc
        let l = docToFormat (wid-ind) indDoc
        let indList = List.map (indentFormat ind) l
        List.filter (fun (form:Format) -> form.width <= wid) indList
    |Above(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2
        //List.sort (List.map (fun (x,y) -> x >-< y) (cross dlist1 dlist2))
        List.map (fun (x,y) -> x >-< y) (cross dlist1 dlist2)
    |Beside(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2
        //let crossed = cross dlist1 dlist2
        //let clear = List.filter (fun ((x:Format),(y:Format)) -> (max x.width (x.widthLast+y.width)) <= wid) crossed
        let blist = List.map (fun (x,y) -> x >|< y) (cross dlist1 dlist2)
        //blist
        //List.map (fun (x,y) -> x >|< y) crossed
        List.filter (fun (form:Format) -> form.width <= wid) blist        
    |Choice(doc1, doc2) ->
        let dlist1 = docToFormat wid doc1
        let dlist2 = docToFormat wid doc2
        List.concat [dlist1; dlist2]
let pretty wid doc =
    let list1 = docToFormat (wid) doc
    (List.rev list1).Head.txtstr 0 ""