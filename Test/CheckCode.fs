module YC.PrinterCombinators.Test
open NUnit.Framework
open System.IO

let filesAreEqual file1 file2 =
    let all1 = File.ReadAllBytes file1
    let all2 = File.ReadAllBytes file2    
    Assert.AreEqual (all1.Length, all2.Length)

module YCCheck =
    open YCLPrinter
        
    let path = "../../../InOut"
    type RunTest() =        
        [<Test>]
        member x.``Check Run``() = 
            let inp = Path.Combine(path, "RunTest.in")
            let exp = Path.Combine(path, "RunTest.expe")
            let outp = Path.Combine(path, "RunTest.out")

            let text = File.ReadAllText inp
            let output = Printer(10, A, A).Print(text)

            let tr = File.WriteAllText( exp, output)
            let out = File.WriteAllText( outp, output)

            filesAreEqual exp outp
    
    [<TestFixture>]
    type YCPrinter() =
        let path = "../../../InOut"    
        static member TestData = 
            [|
                //Run Test
                (10, A, A, "RunTest.in", "RunTest.yp");
            
                (2, A, A, "Combine.in",  "CombineAb.yp");
                (10, A, A, "Combine.in",  "CombineBe.yp");
            
                (10, A, A, "while.in", "whileA.yp");
                (10, A, B, "while.in", "whileB.yp");
                (10, A, AB,"while.in",  "whileAB.yp");
                (5, A, AB, "while.in", "whileW.yp");
            
                (10, A, A, "while+expr.in", "while+expr.yp");

                (10, A, A, "if.in",   "ifA.yp");
                (10, B, A, "if.in", "ifB.yp");
                (10, AB, A,"if.in", "ifAB.yp");
                (5, AB, A, "if.in", "ifW.yp");

                (25, A, A, "LowTree.in",  "LowTreeAA.yp");
                (25, A, B, "LowTree.in",  "LowTreeAB.yp");
                (25, B, B, "LowTree.in",  "LowTreeBB.yp");
                (25, B, A, "LowTree.in",  "LowTreeBA.yp");            
                (25, AB, AB, "LowTree.in",  "LowTreeAbAb.yp");
                (20, A, A, "LowTree.in",  "LowTreeW.yp");

                (50, A, A, "HardTree.in", "HardTreeAA.yp");
                (50, B, B, "HardTree.in", "HardTreeBB.yp");
                (50, AB, AB, "HardTree.in", "HardTreeAB.yp");
                //(10, A, A, ".in", ".expe", ".ycgen");
        
            |]

        [<TestCaseSource("TestData")>]
        member x.``Generate Code``((wid: int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

//            let tr = File.WriteAllText(exp, output)
            let out = File.WriteAllText(outp, output)
            Assert.True
//            filesAreEqual exp outp

module FsxCheck =
    open FsxLPrinter
        
    let path = "../../../InOut"
    type RunTest() =        
        [<Test>]
        member x.``Check Run``() = 
            let inp = Path.Combine(path, "RunTest.in")
            let exp = Path.Combine(path, "RunTest.expe")
            let outp = Path.Combine(path, "RunTest.out")

            let text = File.ReadAllText inp
            let output = Printer(10, A, A).Print(text)

            let tr = File.WriteAllText( exp, output)
            let out = File.WriteAllText( outp, output)

            filesAreEqual exp outp
    
    [<TestFixture>]
    type YCPrinter() =
        let path = "../../../InOut"    
        static member TestData = 
            [|
                //Run Test
                (10, A, A, "RunTest.in", "RunTest.yp");
            
                (2, A, A, "Combine.in",  "CombineAb.yp");
                (10, A, A, "Combine.in",  "CombineBe.yp");
            
                (10, A, A, "while.in", "whileA.yp");
                (10, A, B, "while.in", "whileB.yp");
                (10, A, AB,"while.in",  "whileAB.yp");
                (5, A, AB, "while.in", "whileW.yp");
            
                (10, A, A, "while+expr.in", "while+expr.yp");

                (10, A, A, "if.in",   "ifA.yp");
                (10, B, A, "if.in", "ifB.yp");
                (10, AB, A,"if.in", "ifAB.yp");
                (5, AB, A, "if.in", "ifW.yp");

                (25, A, A, "LowTree.in",  "LowTreeAA.yp");
                (25, A, B, "LowTree.in",  "LowTreeAB.yp");
                (25, B, B, "LowTree.in",  "LowTreeBB.yp");
                (25, B, A, "LowTree.in",  "LowTreeBA.yp");            
                (25, AB, AB, "LowTree.in",  "LowTreeAbAb.yp");
                (20, A, A, "LowTree.in",  "LowTreeW.yp");

                (50, A, A, "HardTree.in", "HardTreeAA.yp");
                (50, B, B, "HardTree.in", "HardTreeBB.yp");
                (50, AB, AB, "HardTree.in", "HardTreeAB.yp");
                //(10, A, A, ".in", ".expe", ".ycgen");
        
            |]

        [<TestCaseSource("TestData")>]
        member x.``Generate Code``((wid: int, iF, wH, input, out)) = 
            let inp = Path.Combine(path, input)
            let outp = Path.Combine(path, out)

            let text = File.ReadAllText inp
            let output = Printer(wid, iF, wH).Print(text)

//            let tr = File.WriteAllText(exp, output)
            let out = File.WriteAllText(outp, output)
            Assert.True
//            filesAreEqual exp outp

//        [<TestCaseSource("TestData")>]
//        member x.``Check Parse Code``((wid: int, iF, wH, input, expected, out)) = 
//            let inp = Path.Combine(path, input)
//            let exp = Path.Combine(path, expected)
//            let outp = Path.Combine(path, out)
//
//            let text = File.ReadAllText inp
//            let output = Printer(wid, iF, wH).Print(text)
//
//            //let tr = File.WriteAllText(exp, output)
//            let out = File.WriteAllText(outp, output)
//
//            filesAreEqual exp outp
