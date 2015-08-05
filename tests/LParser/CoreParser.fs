module CoreParser

type 'a t = char list -> ('a * char list) list

let (>>=) (p: 'a t) (f: 'a -> 'b t) = fun s ->
  List.concat [for (r, s') in p s -> (f r) s']

let mreturn r = fun s -> [(r, s)]
let lambda    = fun s -> []
let item      = fun s -> match s with [] -> [] | h :: s -> [(h, s)]
let sat cond  = item >>= fun c -> if cond c then mreturn c else lambda

let (>>) p q = p >>= fun _ -> q
let (<<) p q = p >>= fun rs -> q >> mreturn rs

let char c    = sat ((=) c)
let digit     = sat (fun c -> (List.tryFind ((=) c) ['0'..'9']).IsSome) 
let alpha     = sat (fun c ->
  (List.tryFind ((=) c) (List.append ['a'..'z'] ['A'..'Z'])).IsSome)

let (<|>) p q = fun s ->
  match p s with
  | [] -> q s
  | rs -> rs  
let (++) p q = fun s -> List.append (p s) (q s)

let rec many0 p = many1 p <|> mreturn []
and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

let rec symbol cs =
  match cs with
  | [] -> mreturn [] 
  | c::cs' -> char c >> symbol cs' >> mreturn cs

let (~&) (s: string   ) = s.ToCharArray() |> List.ofArray
let (~%) (l: char list) = new string(Array.ofList l)

let map (f: 'a -> 'b) (p: 'a t): 'b t = fun s ->
  p s |> List.map (fun (e, s) -> (f e, s)) 

let rec fold (f: 'a -> 'b -> 'b) (init : 'b) (p: 'a t): 'b t =
  (p >>= fun pRes -> fold f (f pRes init) p) <|> mreturn init

let number = map (fun s -> %s |> System.Int32.Parse) (many1 digit) 
let word   = map (~%) (many1 alpha)
let spaces = many0 (char ' ' <|> char '\t'  <|> char '\010'
                             <|> (symbol &"\r\n" >> mreturn '\n') <|> char '\n')
let sp   f = spaces >> f << spaces 

let paren p =
  let lparen = char '('
  let rparen = char ')'
  sp lparen >>= (fun _ -> p() << sp rparen)
let paren' p = paren (fun _ -> p)  

let cparen p =
  let lcparen = char '{'
  let rcparen = char '}'
  sp lcparen >>= (fun _ -> p() << sp rcparen)
let cparen' p = cparen (fun _ -> p) 