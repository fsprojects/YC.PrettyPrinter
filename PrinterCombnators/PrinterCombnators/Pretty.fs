module Pretty

open Doc
open Format

type Tree = 
    | Empty
    | Node of string * Tree * Tree

let rec treeToDoc tree = 
    match tree with
    | Empty -> Text("E")
    | Node(s, l, r) -> 
        let dl = treeToDoc l
        let dr = treeToDoc r
        let idl = Indent(1, dl)
        let idr = Indent(1, dr)
        Choice(Above(Text(s), Above(idl, idr)), Beside(Text(s), Beside(dl, dr)))

let tooWide wid (format1 : Format) (format2 : Format) = max format1.width (format1.widthLast + format2.width) > wid

let update (form : Format) (map1 : Map<Frame, Format>) = 
    let frame = form.ToFrame
    if map1.ContainsKey(frame) then map1.Add(frame, min form map1.[frame])
    else map1.Add(frame, form)

let mapdropWhile (f : 'a * 'b -> bool) (map) = Map.ofSeq (Seq.skipWhile f map)
let mapmerge (map1 : Map<Frame, Format>) (map2 : Map<Frame, Format>) = 
    Map.fold (fun acc key value -> update value acc) map1 map2

let rec docToFormats wid doc = 
    match doc with
    | Text(str) -> 
        if str.Length <= wid then Map.empty.Add(new Frame(str.Length, str.Length), stringToFormat str)
        else Map.empty
    | Indent(ind, indDoc) -> 
        let l = docToFormats (wid - ind) indDoc
        let indMap = Map.fold (fun acc x y -> update (indentFormat ind y) acc) Map.empty l
        Map.filter (fun key (form : Format) -> form.width <= wid) indMap
    | Above(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let aboveForL (l : Format) = Map.fold (fun acc key r -> update (l >-< r) acc) Map.empty dlist2
        Map.fold (fun acc key l -> mapmerge (aboveForL l) acc) Map.empty dlist1
    | Beside(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let clearlist2 l = Map.filter (fun key (r : Format) -> not (tooWide wid l r)) dlist2
        let besideForL (l : Format) = Map.fold (fun acc key r -> update (l >|< r) acc) Map.empty (clearlist2 l)
        Map.fold (fun acc key l -> mapmerge (besideForL l) acc) Map.empty dlist1
    | Choice(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        mapmerge dlist1 dlist2

