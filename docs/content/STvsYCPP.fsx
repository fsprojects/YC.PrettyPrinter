(*** hide ***)
#I "C:/gsv/projects/YC/YC.PrettyPrinter/tests/Example/bin/Release"
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\bin\YC.PrettyPrinter\YC.PrettyPrinter.dll"
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\FSharpx.Text.StructuredFormat.dll" 
#r @"C:\gsv\projects\YC\YC.PrettyPrinter\tests\Example\bin\Release\app.dll"

(**
Why YC.PrettyPrinter is not just yet another Microsoft.Text.StructuredFormat? Lets compare these two libraries. Final version of the sources presented in this post is available on <a href="https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/tests/Example/Program.fs">GitHub</a>.

First of all, common header. We use custom parser for simple "C-like" language implemented for tests. In the following examples, an input string is formatted by the printer we compose. Notice that the printer changes not only the structure of the code, but the syntax too. The printer is designed in such a manner to better demonstrate the difference between two libraries and wrong syntax of the output should not be considered as a flaw. 

The desired maximum width for the output is specified in a common header: see 'width'. 

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

First step is to implement a printer using StructuredFormat library. This printer breaks the line after "then" and "else" keywords. Description of the combinators used in the example is available <a href="https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Text.StructuredFormat/StructuredFormat.fsi">here</a>.

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

This printer produce the following output:
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

Next, we implement the same printer using YC.PrettyPrinter library. The <a href="http://yaccconstructor.github.io/YC.PrettyPrinter/reference/yc-prettyprinter-structuredformat.html">set of combinators implemented in YC.PrettyPrinter</a> is very similar to the set of StructuredFormat combinators which simplify such migration a lot. All you need to do is to change openings and correct the function 'str' used for conversion of layout to string.

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

Good. The result looks similar, but there is an extra space before the semicolon. The reason for this is that 'rightL' is a stub for migration simplification. Spaces manipulation in YC.PrettyPrinter and StructuredFormat has some differences. To fix it, unbreakable concatenation without spaces (combinator >|<) should be used. 

*)

    and printS ast =
        match ast with
        | Stmt.Write s -> wordL "write" ^^ bracketL(print s)
        | Stmt.Seq(s1, s2) -> (printS s1 >|< wordL ";") @@ printS s2

(**

Outputs are identical after the modifications.

<pre>
//input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(1+3+4)};"
if (1 + 5 + 7)
then
  write (1 + 2);
  write (1 + 2)
else
  write (1 + 3 + 4)
</pre>

As you can see, migration is a piece of cake. But what about differences? Let's specify variadic layout for "if". In this case, line break after "then" and "else" keywords is optional: sometimes we want to add next block without breaking. Result should looks like this example:

<pre>
if cond
then stmt1;
     stmt2
else
     stmt3
</pre>

Such printer can be implemented using StructuredFormat by means of the combinator '(---)' : join, possible break with indent=2.

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

Let's format some examples.

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

Well, it seems that everything works just fine. 

However, in some cases we can get a slightly incorrect output. Consider the following example. You can notice that "then" and "else" blocks are formatted differently. Many code styles consider this a bad practice suggesting to choose one style for the given statement. 

<pre>
// input: "if (1+5+7) { write(1+2); write(1+2)} else{ write(veryVeryVeryVeryLongVarName+222+3+4)};"
if (1 + 5 + 7)
then write (1 + 2);
     write (1 + 2)
else
  write (veryVeryVeryVeryLongVarName
         + 222 + 3 + 4)
</pre>

Unfortunately, it is impossible to synchronize line breaks using StructuredFormat. YC.PrettyPrinter, on the other hand, provides choice combinator which come in handy in this situation. 

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

As you can see, the output is formatted exactly as we desired. This demonstrates that combinators implemented in YC.PrettyPrinter are more comprehensive. Also, YC.PrettyPrinter produces global optimal result: the code is formatted in such a way that it has required width and minimal height. 

Output for <code>"if (1+5+7) { write(longVarName+longVarName); write(longVarName+longVarName)} else{ write(longVarName+longVarName)};"</code> is a great demonstration of this property. In this example the width for layout is 25 for both libraries.

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

The output of StructuredFormat exceeds maximum possible width (the longest line consists of 37 symbols) whereas the result generated by YC.PrettyPrinter is of proper width and has the minimal possible height such that it does not exceed required width.

We conclude with the small note about performance. Because of the global optimality of the result produced, YC.PrettyPrinter needs significant time to process an input which makes it unsuitable for fast printing of huge amount of code. We compared the printer based on StructuredFormat (P1) with the printer implemented using YC.PrettyPrinter (P2) which is a result of naive migration from StructuredFormat.
Both printers were used to print the results of SQL translation (represented as an Abstract Syntax Tree) as part of enterprise project. It took 2.5 sec to print the AST of 10000 loc and 13 sec to print the AST of 20000 loc with P2. With P1 both examples were printed in 0.5 sec. 

To sum up, if you need fast printer, then StructuredFormat should be used, but if your goal is a really pretty output, then YC.PrettyPrinter is better choice.

*)