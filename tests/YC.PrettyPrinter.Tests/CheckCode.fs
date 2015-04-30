module YC.PrettyPrinter.Tests
open NUnit.Framework
open System.IO

let path = "../../../InOut"
   
let filesAreEqual file1 file2 =
    let all1 = File.ReadAllBytes file1
    let all2 = File.ReadAllBytes file2    
    Assert.AreEqual (all1.Length, all2.Length)

module ``YC Prnters`` =
    open YCLPrinter            
    [<TestFixture>]
    type ``YCPrinter Generator``() = 
        static member TestData = 
            [|
                //Run Test
                (10, A, A, "RunTest.in", "RunTest.ycp");
            
                (2, A, A, "Combine.in",  "CombineAb.ycp");
                (10, A, A, "Combine.in",  "CombineBe.ycp");
            
                (10, A, A, "while.in", "whileA.ycp");
                (10, A, B, "while.in", "whileB.ycp");
                (15, A, AB,"while.in",  "whileAB.ycp");
                (5, A, AB, "while.in", "whileW.ycp");
            
                (10, A, A, "while+expr.in", "while+expr.ycp");

                (10, A, A, "if.in",   "ifA.ycp");
                (10, B, A, "if.in", "ifB.ycp");
                (10, AB, A,"if.in", "ifAB.ycp");
                (5, AB, A, "if.in", "ifW.ycp");

                (25, A, A, "LowTree.in",  "LowTreeAA.ycp");
                (25, A, B, "LowTree.in",  "LowTreeAB.ycp");
                (25, B, B, "LowTree.in",  "LowTreeBB.ycp");
                (25, B, A, "LowTree.in",  "LowTreeBA.ycp");            
                (25, AB, AB, "LowTree.in",  "LowTreeAbAb.ycp");
                (20, A, A, "LowTree.in",  "LowTreeW.ycp");

                (50, A, A, "HardTree.in", "HardTreeAA.ycp");
                (50, B, B, "HardTree.in", "HardTreeBB.ycp");
                (50, AB, AB, "HardTree.in", "HardTreeAB.ycp");
                //(10, A, A, ".in", ".expe", ".ycgen");
        
            |]
        [<Test>]
        member x.``Check Run``() = 
            let inp = Path.Combine(path, "RunTest.in")
            let outp = Path.Combine(path, "RunTest.run")

            let text = File.ReadAllText inp
            let output = Printer(10, A, A).Print(text)

            let out = File.WriteAllText( outp, output)
            ()

        [<TestCaseSource("TestData")>]
        member x.``Generate Code``((wid: int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

//            let tr = File.WriteAllText(exp, output)
            let out = File.WriteAllText(outp, output)
            ()
//            filesAreEqual exp outp

module ``SFormat Generator`` =
    open FsxLPrinter
           
    [<TestFixture>]
    type ``SFormatPrinter``() =   
        static member TestData = 
            [|
                //Run Test
                (10, A, A, "RunTest.in", "RunTest.fxp");
            
                (2, A, A, "Combine.in",  "CombineAb.fxp");
                (10, A, A, "Combine.in",  "CombineBe.fxp");
            
                (10, A, A, "while.in", "whileA.fxp");
                (10, A, B, "while.in", "whileB.fxp");
                (15, A, AB,"while.in",  "whileAB.fxp");
                (5, A, AB, "while.in", "whileW.fxp");
            
                (10, A, A, "while+expr.in", "while+expr.fxp");

                (10, A, A, "if.in",   "ifA.fxp");
                (10, B, A, "if.in", "ifB.fxp");
                (10, AB, A,"if.in", "ifAB.fxp");
                (5, AB, A, "if.in", "ifW.fxp");

                (25, A, A, "LowTree.in",  "LowTreeAA.fxp");
                (25, A, B, "LowTree.in",  "LowTreeAB.fxp");
                (25, B, B, "LowTree.in",  "LowTreeBB.fxp");
                (25, B, A, "LowTree.in",  "LowTreeBA.fxp");            
                (25, AB, AB, "LowTree.in",  "LowTreeAbAb.fxp");
                (20, A, A, "LowTree.in",  "LowTreeW.fxp");

                (50, A, A, "HardTree.in", "HardTreeAA.fxp");
                (50, B, B, "HardTree.in", "HardTreeBB.fxp");
                (50, AB, AB, "HardTree.in", "HardTreeAB.fxp");
                //(10, A, A, ".in", ".expe", ".ycgen");
        
            |]
        
        [<Test>]
        member x.``Check Run``() = 
            let inp = Path.Combine(path, "RunTest.in")
            let outp = Path.Combine(path, "RunTest.run")

            let text = File.ReadAllText inp
            let output = Printer(10, A, A).Print(text)

            let out = File.WriteAllText( outp, output)
            ()

        [<TestCaseSource("TestData")>]
        member x.``Generate Code``((wid: int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

            let out = File.WriteAllText(outp, output)
            ()


module CheckEquals =
    [<TestFixture>]
    type ``Equals Code``() = 
        static member TestData = 
            [|
                //Run Test
                ("RunTest.ycp", "RunTest.fxp");
            
                ("CombineAb.ycp",  "CombineAb.fxp");
                ("CombineBe.ycp",  "CombineBe.fxp");
            
                ("whileA.ycp", "whileA.fxp");
                ("whileB.ycp", "whileB.fxp");
                ("whileAB.ycp",  "whileAB.fxp");
                ("whileW.ycp", "whileW.fxp");

                ("while+expr.ycp", "while+expr.fxp");

                ("ifA.ycp",   "ifA.fxp");
                ("ifB.ycp", "ifB.fxp");
                ("ifAB.ycp", "ifAB.fxp");
                ("ifW.ycp", "ifW.fxp");

                ("LowTreeAA.ycp",  "LowTreeAA.fxp");
                ("LowTreeAB.ycp",  "LowTreeAB.fxp");
                ("LowTreeBB.ycp",  "LowTreeBB.fxp");
                ("LowTreeBA.ycp",  "LowTreeBA.fxp");            
                ("LowTreeAbAb.ycp",  "LowTreeAbAb.fxp");
                ("LowTreeW.ycp",  "LowTreeW.fxp");

                ("HardTreeAA.ycp", "HardTreeAA.fxp");
                ("HardTreeBB.ycp", "HardTreeBB.fxp");
                ("HardTreeAB.ycp", "HardTreeAB.fxp");
                //(10, A, A, ".in", ".expe", ".ycgen");
        
            |]

        [<TestCaseSource("TestData")>]
        member x.``Are Equals``((f1, f2)) = 
            let усpath = Path.Combine(path, f1)
            let fxpath = Path.Combine(path, f2)

            let file1 = File.ReadAllBytes(усpath)
            let file2 = File.ReadAllBytes(fxpath)

            Assert.AreEqual(file1.Length, file2.Length)

module Perfomanse =
    let [<Test>] ``Test`` () =
        let timer = new System.Diagnostics.Stopwatch()
        let file = Path.Combine(path,"PerfomanceTree.in")
        let fsxPrinter = FsxLPrinter.Printer(50, FsxLPrinter.AB, FsxLPrinter.AB)
        let ycPrinter = YCLPrinter.Printer(50, YCLPrinter.AB, YCLPrinter.AB)

        let text = File.ReadAllText file

        timer.Start()
        let str1 = fsxPrinter.Print(text)
        timer.Stop()
        let timeFX = timer.Elapsed.TotalMilliseconds //ElapsedMilliseconds
    
        timer.Restart()
        let str2 = ycPrinter.Print(text)
        timer.Stop()
    
        let timeYC = timer.Elapsed.TotalMilliseconds //ElapsedMilliseconds
        
        File.WriteAllText(Path.Combine(path,"PerfomanceTree.fxp"), str1)
        File.WriteAllText(Path.Combine(path,"PerfomanceTree.ycp"), str2)

        Assert.AreEqual(timeFX, timeYC)