module YC.PrinterCombinators.Tests
open NUnit.Framework
open System.IO

let path = "../../../InOut"
   
let filesAreEqual file1 file2 =
    let all1 = File.ReadAllBytes file1
    let all2 = File.ReadAllBytes file2
    Assert.AreEqual (all1.Length, all2.Length)

module ``1YC Prnters`` =
    open YC.PrettyPrinter.Tests.YCLPrinter

    [<TestFixture>]
    type ``YCPrinter Generator``() = 
        static member TestData = 
            [|
                (10, A, A, "RunTest.in", "RunTest.ycp");
            
                (2, A, A, "Combine.in",  "CombineAb.ycp");
                (10, A, A, "Combine.in",  "CombineBe.ycp");
            
                (10, A, A, "while.in", "whileA.ycp");
                (10, A, B, "while.in", "whileB.ycp");
                (10, A, AB,"while.in",  "whileAB.ycp");
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
        member x.``Generate Code``((wid : int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

            let out = File.WriteAllText(outp, output)
            ()

module ``2SFormat Generator`` =
    open YC.PrettyPrinter.Tests.FsxLPrinter
           
    [<TestFixture>]
    type ``SFormatPrinter``() =   
        static member TestData = 
            [|
                (10, A, A, "RunTest.in", "RunTest.fxp");
            
                (2, A, A, "Combine.in",  "CombineAb.fxp");
                (10, A, A, "Combine.in",  "CombineBe.fxp");
            
                (10, A, A, "while.in", "whileA.fxp");
                (10, A, B, "while.in", "whileB.fxp");
                (10, A, AB,"while.in",  "whileAB.fxp");
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
        member x.``Generate Code``((wid : int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

            let out = File.WriteAllText(outp, output)
            ()


module ``3CheckEquals`` =
    [<TestFixture>]
    type ``Equals Code``() = 
        static member TestData = 
            [|
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
            |]

        [<TestCaseSource("TestData")>]
        member x.``Are Equals``((f1, f2)) = 
            let усpath = Path.Combine(path, f1)
            let fxpath = Path.Combine(path, f2)

            let file1 = File.ReadAllBytes(усpath)
            let file2 = File.ReadAllBytes(fxpath)

            if file1.Length <> file2.Length 
            then printfn "Difference between %A, and %A" f1 f2

module ``4xSpeed`` =
    open YC.PrettyPrinter.Tests
    [<TestFixture>]
    type ``xSpeedTest``() = 
        static member TestData = 
            [|
                ("xSpeed/x1.in");
                ("xSpeed/x2.in");
                ("xSpeed/x3.in");
                ("xSpeed/x4.in");
                ("xSpeed/x5.in");
                ("xSpeed/x6.in");
                ("xSpeed/x7.in");
                ("xSpeed/x8.in");
            |]

        [<TestCaseSource("TestData")>]
        member  x.``Test`` (spFile) =
            let timer = new System.Diagnostics.Stopwatch()
            let file = Path.Combine(path, spFile)
            let fsxPrinter = FsxLPrinter.Printer(50, FsxLPrinter.AB, FsxLPrinter.AB)
            let ycPrinter = YCLPrinter.Printer(50, YCLPrinter.AB, YCLPrinter.AB)

            let text = File.ReadAllText file
            let mutable timeFX = 0.0
            let mutable timeYC = 0.0
            for i in 0..9 do
                timer.Start()
                let str1 = fsxPrinter.Print(text)
                timer.Stop()
                timeFX <- timeFX + timer.Elapsed.TotalMilliseconds
                
                timer.Restart()
                let str2 = ycPrinter.Print(text)
                timer.Stop()

                timeYC <- timeYC + timer.Elapsed.TotalMilliseconds
                timer.Reset()
            printfn "xSpeed with %A \n Text.StructuredFormat: %A\n YC.PrettyPrinter: %A" spFile (timeFX / 10.0) (timeYC / 10.0)

module ``5Perfomanse`` =
    open YC.PrettyPrinter.Tests
    let [<Test>] ``5Test`` () =
        let timer = new System.Diagnostics.Stopwatch()
        let file = Path.Combine(path,"PerfomanceTree.in")
        let fsxPrinter = FsxLPrinter.Printer(50, FsxLPrinter.AB, FsxLPrinter.AB)
        let ycPrinter = YCLPrinter.Printer(50, YCLPrinter.AB, YCLPrinter.AB)

        let text = File.ReadAllText file

        timer.Start()
        let str1 = fsxPrinter.Print(text)
        timer.Stop()
        let timeFX = timer.Elapsed.TotalMilliseconds
    
        timer.Restart()
        let str2 = ycPrinter.Print(text)
        timer.Stop()
    
        let timeYC = timer.Elapsed.TotalMilliseconds
        
        File.WriteAllText(Path.Combine(path,"PerfomanceTree.fxp"), str1)
        File.WriteAllText(Path.Combine(path,"PerfomanceTree.ycp"), str2)
        if timeFX <> timeYC 
        then printfn "Difference between timeFX %A, and timeYC %A" timeFX timeYC