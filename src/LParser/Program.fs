module Program

open Stmt.Parser
open CoreParser

open System.IO

[<EntryPoint>]
let main argv =
  if argv.Length < 1 then printfn "Not enough arguments.\n"; exit 1
  let inStream = new StreamReader(argv.[0])
  let text = inStream.ReadToEnd ()
  inStream.Dispose ()
  let p = parse () &text |> List.head |> fst
  let output = VerticalPrinter.printer p
  if argv.Length < 2 then printfn "%s\n" output; exit 0
  let outStream = new StreamWriter(argv.[1])
  outStream.Write(output)
  outStream.Dispose ()
  0
