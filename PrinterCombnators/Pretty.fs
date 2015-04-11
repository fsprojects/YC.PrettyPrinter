module Pretty

open Doc
open Format
open System.Collections.Generic
open System.Linq
///Updates map with choosing a min format
let inline update (form : Format) (map1 : Dictionary<Frame, Format>) = 
    let frame = form.ToFrame
    if map1.ContainsKey(frame)
    then map1.[frame] <- min form map1.[frame]
    else map1.Add(frame, form)

///Insert format to map if format is suitable. If map empty then insert non-suitable format.
let inline checkUpdate wid (form : Format) (map1 : Dictionary<Frame, Format>) = 
    if map1.Count = 0 || form.isSuitable wid
    then update form map1

     
let inline mIter f d1 (d2:Dictionary<_,_>) =
    for kvp in d2 do f kvp.Value d1

///Insert in in map1 elements from map2
let inline mapmerge (map1 : Dictionary<Frame, Format>) (map2 : Dictionary<Frame, Format>) =    
    mIter update map1 map2

///Main function that tansform Doc to variants of formats
let rec docToFormats wid doc =
    let res = new Dictionary<_,_>()
    match doc with
    | Text(str) -> update (stringToFormat str) res
    | Indent(ind, indDoc) -> 
        let l = docToFormats (wid - ind) indDoc
        mIter (fun y acc -> checkUpdate wid (indentFormat ind y) acc) res l
    | Above(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let aboveForL (l : Format) = mIter (fun r acc -> update (l >-< r) acc) res dlist2        
        for kvp in dlist1 do aboveForL kvp.Value
    | Beside(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let besideForL (l : Format) = mIter (fun r acc -> checkUpdate wid (l >|< r) acc) res dlist2
        for kvp in dlist1 do besideForL kvp.Value
    |Fill(doc1, shift, doc2) ->
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let fillForL (l : Format) = mIter (fun r acc -> checkUpdate wid (Format.addFill(l,r,shift)) acc) res dlist2
        for kvp in dlist1 do fillForL kvp.Value
    | Choice(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2        
        mapmerge dlist1 dlist2
        mapmerge res dlist1
    res

///Get pretty format         
let pretty (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    try 
        Seq.minBy (fun (y : Format) -> y.height) myMap.Values
    with :? System.ArgumentException -> stringToFormat ""

///Get suitable and min
let best wid (map1:Dictionary<Frame, Format>) =
    let mutable min = map1.Values.First()//map1.GetEnumerator().Current.Value
    //let fd = map1.Values.GetEnumerator().Current
    for x in map1.Values do
        if (x.totalW < min.totalW) //(x.isSuitable wid && x.height < min.height) || (not (min.isSuitable wid) && x.totalW < min.totalW)
        then min <- x
    min
    
///Get pretty format         
let prettyPrints (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    
    for x in myMap do printfn "tw %A" x.Value.totalW 
    
    printfn"%A" myMap.Count
//    Seq.fold (fun (acc:Format) (a :KeyValuePair<Frame, Format>) ->  
//            if a.Value.isSuitable resultWidth || a.Value.height < acc.height then a.Value else acc) (myMap.First().Value) myMap
    best resultWidth myMap 
    |> (fun x -> x.txtstr 0 "")