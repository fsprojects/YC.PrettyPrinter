module YC.PrettyPrinter.Pretty

open YC.PrettyPrinter.Doc
open YC.PrettyPrinter.Format
open System.Collections.Generic
open System.Linq

///Updates map with choosing a min format.
let inline update (form : Format) (map1 : Dictionary<Frame, Format>) = 
    let frame = form.ToFrame
    if map1.ContainsKey(frame)
    then map1.[frame] <- min form map1.[frame]
    else map1.Add(frame, form)

///Insert format to map if format is suitable. If map empty then insert non-suitable format.
let checkUpdate wid (form : Format) (map1 : Dictionary<Frame, Format>) = 
    let mutable min = map1.First()
    if form.isSuitable wid
    then update form map1
   
let mIter f d1 (d2 : Dictionary<_,_>) =
    for kvp in d2 do f kvp.Value d1
 
let sortUpdate wid (result : Dictionary<Frame,Format>) (d2 : Dictionary<Frame, Format>) =
    let mutable elem = d2.Values.First()
    for x in d2.Values do
        if x.isSuitable wid
        then update x result
        elif x.totalW < elem.totalW 
        then elem <- x
    update elem result

///Insert in in map1 elements from map2.
let mapmerge (map1 : Dictionary<Frame, Format>) (map2 : Dictionary<Frame, Format>) =    
    mIter update map1 map2

let cacheMap = new Dictionary<Doc, Dictionary<Frame, Format>>()

///Main function that tansform Doc to variants of formats.
let rec docToFormats wid doc =
    let res = new Dictionary<_,_>()
    match doc with
    | Text(str) -> update (stringToFormat str) res
    | Indent(ind, indDoc) ->
        let l = cacheContains (wid - ind) indDoc
        mIter (fun y acc -> update (indentFormat ind y) acc) res l
    | Above(doc1, doc2) -> 
        let dlist1 = cacheContains wid doc1
        let dlist2 = cacheContains wid doc2
        let aboveForL (l : Format) = mIter (fun r acc -> update (l >-< r) acc) res dlist2        
        for kvp in dlist1 do aboveForL kvp.Value
    | Beside(doc1, doc2) -> 
        let dlist1 = cacheContains wid doc1
        let dlist2 = cacheContains wid doc2
        let besideForL (l : Format) = mIter (fun r acc -> update (l >|< r) acc) res dlist2
        for kvp in dlist1 do besideForL kvp.Value
    |Fill(doc1, shift, doc2) ->
        let dlist1 = cacheContains wid doc1
        let dlist2 = cacheContains wid doc2
        let fillForL (l : Format) = mIter (fun r acc -> update (Format.addFill(l, r, shift)) acc) res dlist2
        for kvp in dlist1 do fillForL kvp.Value
    | Choice(doc1, doc2) -> 
        let dlist1 = cacheContains wid doc1
        let dlist2 = cacheContains wid doc2
        mapmerge dlist1 dlist2
        sortUpdate wid res dlist1
    res

and cacheContains wid doc =
    if cacheMap.ContainsKey(doc) 
    then cacheMap.[doc] 
    else 
        let cacheForm = docToFormats wid doc
        cacheMap.Add(doc, cacheForm)
        cacheForm

///Get pretty format.
let pretty (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    try 
        Seq.minBy (fun (y : Format) -> y.height) myMap.Values
    with :? System.ArgumentException -> stringToFormat ""

///Get suitable and min
let best wid (map1 : Dictionary<Frame, Format>) =
    let mutable value = map1.Values.First()
    for x in map1.Values do
        let suit = x.isSuitable wid
        if (suit && x.height < value.height) || (not suit && x.totalW < value.totalW)
        then value <- x
    value
    
///Get pretty format with Best       
let prettyPrints (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    best resultWidth myMap
    |> (fun x -> x.txtstr 0 "")