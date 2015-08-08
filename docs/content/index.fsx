(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
YC.PrettyPrinter
======================

YC.PrettyPrinter is an F# implementation of printer combinator library based on the paper
<a href="http://link.springer.com/chapter/10.1007%2F978-3-662-46823-4_21">Polynomial-Time 
Optimal Pretty-Printing Combinators with Choice</a>. The library contains 5 combinators, 
sufficient for creation of pretty printer for optimal representation of code. The interface
for an easy migration from <a href="https://github.com/jack-pappas/FSharp.Text.StructuredFormat">FSharp.Text.StructuredFormat</a>
is provided and can be found 
<a href="http://yaccconstructor.github.io/YC.PrettyPrinter/reference/yc-prettyprinter-structuredformat.html">here</a>.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The YC.PrettyPrinter library can be <a href="https://nuget.org/packages/YC.PrettyPrinter">installed from NuGet</a>:
      <pre>PM> Install-Package YC.PrettyPrinter</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Samples & documentation
-----------------------

The library comes with documentation and include [Getting Started tutorial](tutorial.html), 
as well as an [API Reference](reference/index.html)
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/YaccConstructor/YC.PrettyPrinter/tree/master/docs/content
  [gh]: https://github.com/YaccConstructor/YC.PrettyPrinter
  [issues]: https://github.com/YaccConstructor/YC.PrettyPrinter/issues
  [readme]: https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/README.md
  [license]: https://github.com/YaccConstructor/YC.PrettyPrinter/blob/master/LICENSE
*)
