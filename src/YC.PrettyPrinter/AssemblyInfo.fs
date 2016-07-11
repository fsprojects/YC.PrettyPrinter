namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("YC.PrettyPrinter")>]
[<assembly: AssemblyProductAttribute("YC.PrettyPrinter")>]
[<assembly: AssemblyDescriptionAttribute("Pretty printing library")>]
[<assembly: AssemblyVersionAttribute("0.0.5")>]
[<assembly: AssemblyFileVersionAttribute("0.0.5")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.5"
    let [<Literal>] InformationalVersion = "0.0.5"
