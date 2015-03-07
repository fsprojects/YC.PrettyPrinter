module Test

open Format
open Doc

let p  = stringToFormat ("rerererererere \n rerere")
let p2  = stringToFormat ("qqqqqqqqqqqq \n qqqq")
let f = p >|< p2
printfn "%A" (f.txtstr 2 "finish")
printfn "%A" (spaces 5)

let addOne x = x + 1
let timesTwo x = 2 * x
let Compose1 = addOne << timesTwo

// Result is 5
let result1 = Compose1 2