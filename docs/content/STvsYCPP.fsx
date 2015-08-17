(*** hide ***)
#I "C:/gsv/projects/YC/YC.PrettyPrinter/tests/Example/bin/Release"
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\bin\YC.PrettyPrinter\YC.PrettyPrinter.dll"
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\FSharpx.Text.StructuredFormat.dll" 
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\app.dll"

(**
Why YC.PrettyPrinter is not yiet another Microsoft.Text.StructuredFormat? Lets try to compare this two libraries. Final version of sources presented in this post is available on <a href="https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/tests/Example/Program.fs">GitHub</a>.

First of all, common header. I use custom parser for simple "C-like" language implemented for tests. In examples below I do not aimed to format input string. So, input and output syntax may be different. Also let we specify width for result layout.

*)

module Example

open Expr
open Stmt
open Stmt.Parser
open CoreParser

let ast = 
    &"if (1+5+7) { write(1+2)} else{ write(1+3+4)};"
    |> Stmt.Parser.parse () 
    |> List.head
    |> fst

let width = 25

(**

First step is creation of printer with braking after "then" and "else" by using StructuredFormat. Description of combinators is available <a href="https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Text.StructuredFormat/StructuredFormat.fsi">here</a>.

*)

module SF =
    open Microsoft.FSharp.Text.StructuredFormat
    open Microsoft.FSharp.Text.StructuredFormat.LayoutOps

    let rec print ast =
        match ast with
        | Expr.BinOp(x, y, z) -> print y ++ wordL (string x) ^^ print z
        | Expr.Num n -> string n |> wordL
        | Expr.Var n -> string n |> wordL
    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" @@-- printS t
            let _f = wordL "else" @@-- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 
    
    let str ast = 
        printS ast
        |> Display.layout_to_string {FormatOptions.Default with PrintWidth = width}

(**

Output is formatted input string:
<pre>
//input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then
  write (1 + 2);
  write (1 + 2)
else
  write (1 + 3 + 4)
</pre>

*)



(**

The next step is migration to YC.PrettyPrinter. Since the <a href="http://yaccconstructor.github.io/YC.PrettyPrinter/reference/yc-prettyprinter-structuredformat.html">set of combinators implemented in YC.PrettyPrinter</a> is similar to the set of StructuredFormat combinators, you need only change openings and correct final function for layout ot string converting.   

*)

module YCPP =
    open YC.PrettyPrinter
    open YC.PrettyPrinter.StructuredFormat

    let rec print ast =
        match ast with
        | Expr.BinOp(x, y, z) -> print y ++ wordL (string x) ^^ print z
        | Expr.Num n -> string n |> wordL
        | Expr.Var n -> string n |> wordL
    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" @@-- printS t
            let _f = wordL "else" @@-- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 

    let str ast = 
        printS ast
        |> Pretty.print width

(**
Run it.
<pre>
//input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then
  write (1 + 2) ;
  write (1 + 2)
else
  write (1 + 3 + 4)
</pre>

Good. We get similar result but there is some problems with semicolon: extra space is added. It is because rightL is stub for migration simplification. Spaces manipulation in YC.PrettyPrinter and StructuredFormat has some differences. Well, let we try to fix it: wordL and unbrakable concatenation without spaces (combinator >|<) can help to solve the problem. 

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2

(**

<pre>
//input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then
  write (1 + 2);
  write (1 + 2)
else
  write (1 + 3 + 4)
</pre>

Ather modifications outputs are identical.

Well. Migration is pretty simple. But what about differences? Let try to cpecify variadic layout for "if". It is not necessary to add line braking after "then" and "else" always. Sometymes we want to add next block without braking. Result should looks like this example:

<pre>
if cond
then stmt1;
     stmt2
else
     stmt3
</pre>

To implement such printer using StructuredFormat we can use combinator (---) : join, possible break with indent=2.

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 ^^ rightL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = wordL "then" --- printS t
            let _f = wordL "else" --- printS f
            (wordL "if" ^^ bracketL(print c)) @@ _t @@ _f 

(**

Let try to format some examples.

<pre>
// input: "if (1+5+7) { write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then write (1 + 2)
else write (1 + 3 + 4)
</pre>


<pre>
//input: 
//"if (1+5+7) { write(longVarName+longVarName); write(longVarName+longVarName)}"
//+" else{ write(longVarName+longVarName)};"
//
if (1 + 5 + 7)
then
  write (longVarName + longVarName);
  write (longVarName + longVarName)
else
  write (longVarName + longVarName)
</pre>

<pre>
// input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then write (1 + 2);
     write (1 + 2)
else write (1 + 3 + 4)
</pre>

Well, seems that all works as we want. 

<pre>
// input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(veryVeryVeryVeryLongVarName+222+3+4)};"
if (1 + 5 + 7)
then write (1 + 2);
     write (1 + 2)
else
  write (veryVeryVeryVeryLongVarName
         + 222 + 3 + 4)
</pre>

In some cases we can get slightly incorrect output. It happens because we can not sinchronize brakings after "then" and "else" and we can not solve this problem using StructuredFormat. But YC.PrettyPrinter can help us because it contais choice combinator. 

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2
        | Stmt.If(c,t,f) -> 
            let _t = printS t
            let _f = printS f
            // We can specify "template" for "if-then-else" layout.
            let ifTpl f = 
                (wordL "if" ^^ bracketL(print c)) @@ (wordL "then" |> f <| _t) @@ (wordL "else" |> f <| _f )            
            // Choice: all unbrekable or all with braking.
            (ifTpl (^^)) >//< (ifTpl (@@--))

(**

Output for <code>"if (1+5+7) { write(1+2); write(1+2)} else{ write(veryVeryVeryVeryLongVarName+222+3+4)};"</code>:
<pre>
if (1 + 5 + 7)
then
  write (1 + 2);
  write (1 + 2)
else
  write (veryVeryVeryVeryLongVarName
         + 222 + 3 + 4)
</pre>

Well. It is what we want. So, combinators which implemented in YC.PrettyPrinter is more comprehensive. Also, they are global optimal: result is layout with required width and minimal height. 
Output for <code>"if (1+5+7) { write(longVarName+longVarName); write(longVarName+longVarName)} else{ write(longVarName+longVarName)};"</code> is good demonstartion of this property. Width for layout is 25 for both libraries.

<pre>
//StructuredFormat
if (1 + 5 + 7)
then
  write (longVarName + longVarName);
  write (longVarName + longVarName)
else
  write (longVarName + longVarName)

//YC.PrettyPrinter
if (1 + 5 + 7)
then
  write (longVarName
         + longVarName);
  write (longVarName
         + longVarName)
else
  write (longVarName
         + longVarName)
</pre>

As you can see, width of result generated by StructuredFormat is greater then 25 and width of result generated by YC.PrettyPrinter is not greater and height is minimal such that do not exceed required width.

Small note about performance. YC.PrettyPrinter is not sutable for fast printing of huge amount of code. We compare printer based on StructuredFormat (P1) with printer on YC.PrettyPrinter (P2) whish is result of naive migration form StructuredFormat.
P1 was implemented for SQL translation results printing (AST) as part of enterprice project. It is nesessary 2.5 sec to print back AST of 10000 loc and 13 sec to print back AST of 20000 loc with P2. With P1 both examples printed in 0.5 sec. 

Final result: if you need fast printer then you should use StructuredFormat, but if your goal is really pretty output then YC.PrettyPrinter is better choice.

*)