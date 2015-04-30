namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("YC.PrettyPrinter")>]
[<assembly: AssemblyProductAttribute("YC.PrettyPrinter")>]
[<assembly: AssemblyDescriptionAttribute("Pretty printing library")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
