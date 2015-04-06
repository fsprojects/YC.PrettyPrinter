module Pretty

open Doc
open Format

let tooWide wid (format1 : Format) (format2 : Format) = max format1.width (format1.widthLast + format2.width) > wid

///Updates map with choosing a min format
let update (form : Format) (map1 : Map<Frame, Format>) = 
    let frame = form.ToFrame
    if map1.ContainsKey(frame) then map1.Add(frame, min form map1.[frame])
    else map1.Add(frame, form)

///Insert format to map if format is suitable. If map empty then insert non-suitable format.
let checkUpdate wid (form : Format) (map1 : Map<Frame, Format>) = 
    if map1.Count = 0 then update form map1
    elif form.isSuitable wid then update form map1
    else map1

///Insert in in map1 elements from map2
let mapmerge (map1 : Map<Frame, Format>) (map2 : Map<Frame, Format>) = 
    Map.fold (fun acc key value -> update value acc) map1 map2
///Заменяем фолды. Выйгрыш в CFA но проигрыш в kernel
let cross wid predicate (map1 : Map<Frame, Format>) (map2 : Map<Frame, Format>) =
    let result = ref Map.empty : Map<Frame, Format> ref
    for x in map1 do
        for y in map2 do
            result := checkUpdate wid (predicate x.Value y.Value) (!result)
    !result  
///Main function that tansform Doc to variants of formats
let rec docToFormats wid doc = 
    match doc with
    | Text(str) -> update (stringToFormat str) Map.empty
    | Indent(ind, indDoc) -> 
        let l = docToFormats (wid - ind) indDoc
        Map.fold (fun acc x y -> checkUpdate wid (indentFormat ind y) acc) Map.empty l
    | Above(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let aboveForL (l : Format) = Map.fold (fun acc key r -> update (l >-< r) acc) Map.empty dlist2
        Map.fold (fun acc key l -> mapmerge (aboveForL l) acc) Map.empty dlist1
    | Beside(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let besideForL (l : Format) = Map.fold (fun acc key r -> checkUpdate wid (l >|< r) acc) Map.empty dlist2
        Map.fold (fun acc key l -> mapmerge (besideForL l) acc) Map.empty dlist1
    | Fill(doc1, shift, doc2) -> // Пока не используется
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let clearlist2 l = Map.filter (fun key (r : Format) -> not (tooWide wid l r)) dlist2
        let fillForL (l : Format) = 
            Map.fold (fun acc key r -> update (Format.addFill (l, r, shift)) acc) Map.empty dlist2
        Map.fold (fun acc key l -> mapmerge (fillForL l) acc) Map.empty dlist1
    | Choice(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        mapmerge dlist1 dlist2

///Get pretty format         
let pretty (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    try 
        Seq.minBy (fun (x : Frame, y : Format) -> y.height) (Map.toSeq myMap) |> fun (x, y) -> y
    with :? System.ArgumentException -> stringToFormat ""