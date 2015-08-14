module YC.PrettyPrinter.Format

open System.Collections.Generic

let spaces n = String.replicate n " "
let nl_skip n s = "\r\n" + spaces n + s

[<Measure>] type height

[<Measure>] type lastWidth

[<Measure>] type totalWidth

[<Measure>] type firstWidth

[<Measure>] type Frame

let cFirst = 10
let cMid = 12
let cLast = 10

let inline newFrame (first: int<firstWidth>) (mid: int<totalWidth>) (last:int<lastWidth>) = 
    (int first <<< cMid + cLast) ||| (int mid <<< cLast) ||| int last 
    |> (*) 1<Frame>

let inline frameFirst (frame: int<Frame>) =
    int frame >>> cMid + cLast
    |> (*) 1<firstWidth>

let inline frameMid (frame: int<Frame>) =
    (int frame <<< cFirst) >>> cMid + cLast
    |> (*) 1<totalWidth>

let inline frameLast (frame: int<Frame>) =
    (int frame <<< cMid + cFirst) >>> cMid + cFirst
    |> (*) 1<lastWidth>

type Format =
    
    val height : int<height>        
    val last : int<lastWidth>
    val mid : int<totalWidth>
    val first : int<firstWidth>
    val txtstr : int -> string -> string
    
    new(firstWidth0, width0, widthLast0, height0, txtstr0) = 
        { height = height0
          last = widthLast0
          mid = width0
          txtstr = txtstr0
          first = firstWidth0 }

    static member (==) (format1 : Format, format2 : Format) = 
        format1.height = format2.height && format1.mid = format2.mid && format1.last = format2.last

    ///Returns if format.width < width.
    member this.isSuitable width = this.totalW <= width
    ///Max width.
    member this.totalW = List.max[int this.first; int this.mid; int this.last]
    ///Frame3d.
    member this.ToFrame = newFrame this.first this.mid this.last
    member this.toString = this.txtstr 0 ""
    
    ///Above Format.
    static member (>-<) (f1 : Format, f2 : Format) = 
        let newFirst = f1.first
        let newMid = 
            max (if f1.height > 1<height> then max (int f1.mid) (int f1.last)  else 0)
                (if f2.height > 1<height> then max (int f2.first) (int f2.mid) else max (int f1.mid) (int f1.last))
            * 1<totalWidth>
        let newLast = 
            if f2.last <> 0<lastWidth> 
            then f2.last
            else f1.last
        let newHeight = f1.height + f2.height
        let newFun = fun n -> f1.txtstr n << nl_skip n << f2.txtstr n
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Beside Format.
    static member (>|<) (f1 : Format, f2 : Format) = 
        let newFirst = 
            (if f1.height <> 1<height> 
             then f1.first
             else f1.first + f2.first)
        let newMid = 
            (if f1.height > 1<height> then f1.mid else 0<totalWidth>)
            |> max (if f1.height = 1<height> && f2.height = 1<height> then f1.mid + f2.mid else 0<totalWidth>)
            |> max (if f2.height > 1<height> 
                    then max (int f1.last + int f2.first) (int f1.last + int f2.mid) * 1<totalWidth> 
                    else f1.mid)
        let newLast = f1.last + f2.last
        let newHeight = f1.height + f2.height - 1<height>
        let newFun = fun n -> f1.txtstr n << f2.txtstr (int f1.last + n)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Fill format.
    static member (>/<) (f1 : Format, f2 : Format) = Format.addFill (f1, f2, 1)
    ///Fill format.
    static member addFill (f1 : Format, f2 : Format, shift : int) = 
        let newFirst = 
            if f1.height <> 1<height> 
            then f1.first
            else f1.first + f2.first
        let newMid =
            (if f1.height = 1<height> && f2.height = 1<height> then f1.mid + f2.mid else 0<totalWidth>)
            |> max (if f1.height > 1<height> && f2.height = 1<height> then f1.mid else 0<totalWidth>)
            |> max (if f1.height = 1<height> && f2.height > 1<height> then f2.mid + shift * 1<totalWidth> else 0<totalWidth>)
            |> max (if f1.height > 1<height> && f2.height > 1<height> 
                    then max ((int f1.last + int f2.first) * 1<totalWidth>) (f2.mid + shift * 1<totalWidth>) 
                    else 0<totalWidth>)
                     
        let newLast = 
            if f2.height <> 1<height> 
            then f2.last + shift * 1<lastWidth>
            else f1.last + f2.last
        let newHeight = f1.height + f2.height - 1<height>
        let newFun = fun n -> f1.txtstr n << f2.txtstr (n + shift)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    //Format comparable realisation.
    interface System.IComparable with
        member this.CompareTo format2 = 
            match format2 with
            | :? Format as f2 -> 
                if (this.height < f2.height) || (this.height = f2.height && this.totalW < f2.totalW) 
                then -1
                elif this.height = f2.height && this.mid = f2.mid && this.last = this.last
                then 0
                else 1
            | _ -> invalidArg "format2" "cannot compare values of different types"

    override x.Equals(yobj) = 
        match yobj with
        | :? Format as y -> x.height = y.height && x.first = y.first && x.mid = y.mid && x.last = y.last
        | _ -> false
    
    override x.GetHashCode() =
        hash (x.height, x.first, x.mid, x.last)

let emptyFormat = new Format(0<firstWidth>, 0<totalWidth>, 0<lastWidth>, 0<height>, fun _ _ -> "")

///Making Format from string.
let stringToFormat (s : string) = 
    let length = s.Length
    new Format(length * 1<firstWidth>, length * 1<totalWidth>, length * 1<lastWidth>, 1<height>, (fun _ p -> s + p))

///Adding indent to given Format.
let indentFormat (h : int) (format : Format) = 
    new Format(h * 1<firstWidth> + format.first, h * 1<totalWidth> + format.mid, h * 1<lastWidth> + format.last, format.height, 
               fun n s -> (spaces h) + format.txtstr (h + n) s)
               