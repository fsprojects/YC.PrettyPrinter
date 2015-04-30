module YC.PrettyPrinter.Doc

open System.Collections.Generic
type Doc = 
    | Text of string
    | Indent of int * Doc
    | Beside of Doc * Doc
    | Above of Doc * Doc
    | Fill of Doc * int * Doc
    | Choice of Doc * Doc
   
    ///Beside
    static member (>|<) (doc1, doc2) = Beside(doc1, doc2)
    ///Above
    static member (>-<) (doc1, doc2) = Above(doc1, doc2)
    ///Fill
    static member (>/<) (doc1, doc2) = Fill(doc1, 0, doc2)
    ///Choice
    static member (>//<) (doc1, doc2) = Choice(doc1, doc2)
    ///Separate
    static member (>||<) (doc1, doc2) = doc1 >|< Text " " >|< doc2