module YC.PrinterCombinators.StructuredFormat
open Doc
/// The empty doc
let emptyL = Text ""
/// Is it the empty doc?
let isEmptyL d = (Text "").Equals(d)
        
/// An uninterpreted leaf, to be interpreted into a string
/// by the layout engine. This allows leaf layouts for numbers, strings and
/// other atoms to be customized according to culture.
let objL o = Text(o.ToString())
/// An string leaf 
let wordL s = Text(s)
/// Join, unbreakable. 
let (^^) doc1 doc2 = doc1 >||< doc2
/// Join, possible break with indent=0
let (++) (doc1:Doc) doc2 = (doc1 >-< doc2) >//< (doc1 >||< doc2)         // >//< (Fill(doc1, 0, doc2))   
/// Join, possible break with indent=1
let (--)  doc1 doc2   = (doc1 >-< Indent(1,doc2)) >//< (doc1 >||< doc2)   // >//< (doc1 >/< doc2)   
/// Join, possible break with indent=2 
let (---) doc1 doc2   = (doc1 >-< Indent(2,doc2)) >//< (doc1 >||< doc2) // >//< (Fill(doc1, 2, doc2))    
/// Join broken with ident=0
let (@@)  doc1 doc2 = Above(doc1, doc2)
/// Join broken with ident=1 
let (@@-) doc1 doc2 = Above(doc1, Indent(1,doc2))   
/// Join broken with ident=2 
let (@@--) doc1 doc2 = Above(doc1, Indent(2,doc2))   

let rec tagListL tagger = function
    | []   -> emptyL
    | [x]   -> x
    | x::xs ->
        (tagger x) >||< tagListL tagger xs
/// Join layouts into a comma separated list.
let commaListL x = tagListL (fun prefixL -> prefixL >|< Text ",") x
          
/// Join layouts into a space separated list.    
let spaceListL x = tagListL (fun prefixL -> prefixL) x
          
/// Join layouts into a semi-colon separated list.
let semiListL x  = tagListL (fun prefixL -> prefixL >|< Text ";") x

/// Join layouts into a list separated using the given Layout.
let sepListL x y = tagListL (fun prefixL -> prefixL >|< x ) y

/// Wrap round brackets around Layout.
let bracketL l = Text "(" >|< l >|< Text ")"
/// Wrap square brackets around layout.    
let squareBracketL x = Text "[" >|< x >|< Text "]"    
/// Wrap braces around layout.        
let braceL         x = (Text "{" >|< x >|< Text "}")  // (Text "{ " >-< x >-< Text"}")  
/// Form tuple of layouts.            
let tupleL xs = bracketL (sepListL (Text ",") xs)
/// Layout two vertically.
let aboveL (doc1:Doc) (doc2:Doc) = doc1 >-< doc2
/// Layout list vertically.    
let aboveListL = function
        | []    -> emptyL
        | [x]   -> x
        | x::ys -> List.fold (fun pre y -> pre @@ y) x ys

/// Layout like an F# option.
let optionL xL = function
        None   -> wordL "None"
        | Some x -> wordL "Some" -- (xL x)
/// Layout like an F# list.    
let listL xL xs = Text "[" ^^ sepListL (Text ";") (List.map xL xs) ^^ Text "]"

/// For limitting layout of list-like sequences (lists,arrays,etc).
/// unfold a list of items using (project and z) making layout list via itemL.
/// If reach maxLength (before exhausting) then truncate.
let boundedUnfoldL
            (itemL     : 'a -> Doc)
            (project   : 'z -> ('a * 'z) option)
            (stopShort : 'z -> bool)
            (z : 'z)
                    maxLength =
          let rec consume n z =
            if stopShort z then [wordL "..."] else
            match project z with
              | None       -> []  (* exhaused input *)
              | Some (x,z) -> if n<=0 then [wordL "..."]               (* hit print_length limit *)
                                      else itemL x :: consume (n-1) z  (* cons recursive... *)
          consume maxLength z  

let unfoldL itemL project z maxLength = boundedUnfoldL  itemL project (fun _ -> false) z maxLength