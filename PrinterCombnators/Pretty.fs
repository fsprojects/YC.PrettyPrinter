module Pretty
//Now all -> Format
open Doc
open Format
open System.Collections.Generic
open System.Linq

///Main function that tansform Doc to variants of formats
let rec docToFormats wid doc =
    match doc with
    | Text(str) -> stringToFormat str
    | Indent(ind, indDoc) -> 
       indentFormat ind (docToFormats (wid-ind) indDoc)
    | Above(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        dlist1 >-< dlist2
    | Beside(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        dlist1 >|< dlist2
    |Fill(doc1, shift, doc2) ->
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        Format.addFill(dlist1,dlist2,shift)
    | Choice(doc1, doc2) -> 
        let dlist1 = docToFormats wid doc1
        let dlist2 = docToFormats wid doc2
        let d1suit = dlist1.isSuitable wid
        let d2suit = dlist2.isSuitable wid        
        match dlist1, dlist2 with
        |_,_ when d1suit && d2suit -> min dlist1 dlist2 
        |_,_ when d1suit && not d2suit -> dlist1 
        |_,_ when not d1suit && d2suit -> dlist1 
        |_,_ when not d1suit && not d2suit -> if dlist1.totalW < dlist2.totalW then dlist1 else dlist2 

///Get pretty format with Best       
let prettyPrints (resultWidth : int) (d : Doc) = 
    let myMap = docToFormats resultWidth d
    myMap.txtstr 0 ""