module Expr

type t = Num of int
       | Var of string
       | BinOp of char * t * t

exception NotOperation 
let op s =
  match s with
  | '+' -> (+)
  | '-' -> (-)
  | '*' -> ( * )
  | '/' -> (/)
  | '%' -> ( % )
  | '^' -> pown
  | _   -> raise NotOperation

module Parser =
  open CoreParser

  exception EmptyList
  let fold1 f s =
    match s with
    | h :: s -> List.fold f h s
    | [] -> raise EmptyList

  let rec pLeftAssoc' opParser pNext =
    opParser >>= fun op ->
    pNext >>= fun r ->
    mreturn (fun l -> BinOp (op, l, r))
  let rec pLeftAssoc opParser pNext =
    let app f x = f x 
    pNext >>= fun l -> fold app l (pLeftAssoc' opParser pNext)
  let rec pRightAssoc opParser pNext =
    pNext >>= fun leftArg ->
    (opParser >>= fun op ->
     (pRightAssoc opParser pNext) >>= fun s ->
       mreturn (BinOp (op, leftArg, s)))
    <|> mreturn leftArg

  let rec parse _ : t CoreParser.t =
    let op1Parser = char '^' |> sp
    let op2Parser = char '*' <|> char '/' <|> char '%' |> sp
    let op3Parser = char '+' <|> char '-' |> sp
    let term = (map Num number) <|> (map Var word) |> sp
    let baseParser = term <|> paren parse
    pLeftAssoc op3Parser (pLeftAssoc op2Parser (pRightAssoc op1Parser baseParser))

let (>>=) a f =
  match a with
  | None -> None
  | Some  a -> f a 
let mreturn a = Some a

let rec calc env e =
  match e with
  | Num n -> Some n
  | Var x -> env  x
  | BinOp (o, x, y) ->
    calc env x >>= fun vx -> 
    calc env y >>= fun vy -> 
    (op o) vx vy |> mreturn 