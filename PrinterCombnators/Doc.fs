module Doc

open System.Collections.Generic

type Doc = 
    | Text of string
    | Indent of int * Doc
    | Beside of Doc * Doc
    | Above of Doc * Doc
    | Fill of Doc * int * Doc
    | Choice of Doc * Doc
    static member (>|<) (doc1, doc2) = Beside(doc1, doc2)
    static member (>-<) (doc1, doc2) = Above(doc1, doc2)
    static member (>/<) (doc1, doc2) = Fill(doc1, 0, doc2)
    static member (>//<) (doc1, doc2) = Choice(doc1, doc2)
    static member (>||<) (doc1, doc2) = doc1 >|< Text " " >|< doc2
