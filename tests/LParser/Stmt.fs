module Stmt

type t = 
       | Read   of string
       | Write  of Expr.t
       | Assign of string * Expr.t
       | Seq    of t * t
       | If     of Expr.t * t * t
       | While  of Expr.t * t

module Parser =
  open CoreParser
  let readP   = symbol &"read"  >> paren' (map (fun x -> Read x) word)
  let writeP  = symbol &"write" >> paren' (map Write (Expr.Parser.parse ()))
  let assignP = (sp word << sp (symbol &":=")) >>= fun x ->
                map (fun e -> Assign (x, e)) (Expr.Parser.parse ())

  let ifP    p = (symbol &"if" |> sp)  >>
                 (Expr.Parser.parse () |> sp) >>= fun e ->
                 p() |> cparen' >>= fun op1 ->
                 (symbol &"else" |> sp) >>
                 (p() |> cparen' |> map (fun op2 -> If (e, op1, op2)))
  let whileP p = (symbol &"while" |> sp)  >>
                 (Expr.Parser.parse () |> sp) >>= fun e ->
                 (p() |> cparen' |> map (fun op -> While (e, op)))

  let term   p = readP <|> writeP <|> assignP <|> ifP p <|> whileP p

  let seqP   p = (term p << sp (char ';')) >>= fun x ->
                 (p() |> sp |> map (fun y -> Seq (x, y)))

  let rec parse _: t CoreParser.t =
    seqP parse <|> term parse