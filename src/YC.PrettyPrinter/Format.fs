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
    val widthInfo : int<Frame>    
    val txtstr : int -> string -> string    
    
    new(firstWidth0, width0, widthLast0, height0, txtstr0) = 
        { height = height0
          widthInfo = newFrame firstWidth0 width0 widthLast0         
          txtstr = txtstr0 }
    new(widthInfo, height0, txtstr0) = 
        { height = height0
          widthInfo = widthInfo         
          txtstr = txtstr0 }

    static member (==) (format1 : Format, format2 : Format) = 
        format1.height = format2.height && frameMid format1.widthInfo = frameMid format2.widthInfo && frameLast format1.widthInfo = frameLast format2.widthInfo

    ///Returns if format.width < width.
    member this.isSuitable width = this.totalW <= width
    ///Max width.
    member this.totalW = 
        int <| frameFirst this.widthInfo
        |> max (int <| frameMid this.widthInfo)
        |> max (int <| frameLast this.widthInfo)
    ///Frame3d.
    member this.ToFrame = this.widthInfo
    member this.toString = this.txtstr 0 ""
    
    ///Above Format.
    static member (>-<) (f1 : Format, f2 : Format) = 
        let newFirst = frameFirst f1.widthInfo
        let newMid = 
            max (if f1.height > 1<height> then max (int <| frameMid f1.widthInfo) (int <| frameLast f1.widthInfo)  else 0)
                (if f2.height > 1<height> then max (int <| frameFirst f2.widthInfo) (int <| frameMid f2.widthInfo) else max (int <| frameMid f1.widthInfo) (int <| frameLast f1.widthInfo))
            * 1<totalWidth>
        let newLast = 
            if frameLast f2.widthInfo <> 0<lastWidth> 
            then frameLast f2.widthInfo
            else frameLast f1.widthInfo
        let newHeight = f1.height + f2.height
        let newFun = fun n -> f1.txtstr n << nl_skip n << f2.txtstr n
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Beside Format.
    static member (>|<) (f1 : Format, f2 : Format) = 
        let newFirst = 
            (if f1.height <> 1<height> 
             then frameFirst f1.widthInfo
             else frameFirst f1.widthInfo + frameFirst f2.widthInfo)
        let newMid = 
            (if f1.height > 1<height> then frameMid f1.widthInfo else 0<totalWidth>)
            |> max (if f1.height = 1<height> && f2.height = 1<height> then frameMid f1.widthInfo + frameMid f2.widthInfo else 0<totalWidth>)
            |> max 
                (if f2.height > 1<height> 
                then max ((int <| frameLast f1.widthInfo) + (int <| frameFirst f2.widthInfo)) ((int <| frameLast f1.widthInfo) + (int <| frameMid f2.widthInfo)) * 1<totalWidth> 
                else frameMid f1.widthInfo)
        let newLast = frameLast f1.widthInfo + frameLast f2.widthInfo
        let newHeight = f1.height + f2.height - 1<height>
        let newFun = fun n -> f1.txtstr n << f2.txtstr ((int <| frameLast f1.widthInfo) + n)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Fill format.
    static member (>/<) (f1 : Format, f2 : Format) = Format.addFill (f1, f2, 1)
    ///Fill format.
    static member addFill (f1 : Format, f2 : Format, shift : int) = 
        let newFirst = 
            if f1.height <> 1<height> 
            then frameFirst f1.widthInfo
            else frameFirst f1.widthInfo + frameFirst f2.widthInfo
        let newMid =            
            (if f1.height = 1<height> && f2.height = 1<height> then frameMid f1.widthInfo + frameMid f2.widthInfo else 0<totalWidth>)
            |> max (if f1.height > 1<height> && f2.height = 1<height> then frameMid f1.widthInfo else 0<totalWidth>)
            |> max (if f1.height = 1<height> && f2.height > 1<height> then frameMid f2.widthInfo + shift * 1<totalWidth> else 0<totalWidth>)
            |> max (if f1.height > 1<height> && f2.height > 1<height> 
                    then max (((int <| frameLast f1.widthInfo) + (int <| frameFirst f2.widthInfo)) * 1<totalWidth>) (frameMid f2.widthInfo + shift * 1<totalWidth>) 
                    else 0<totalWidth>)                      
        let newLast = 
            if f2.height <> 1<height> 
            then frameLast f2.widthInfo + shift * 1<lastWidth>
            else frameLast f1.widthInfo + frameLast f2.widthInfo
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
                elif (this.height = f2.height && frameMid this.widthInfo = frameMid f2.widthInfo && frameLast this.widthInfo = frameLast this.widthInfo) 
                then 0
                else 1
            | _ -> invalidArg "format2" "cannot compare values of different types"

    override x.Equals(yobj) = 
        match yobj with
        | :? Format as y -> x.height = y.height && x.widthInfo = y.widthInfo
        | _ -> false
    
    override x.GetHashCode() =
        hash (x.height, x.widthInfo)

let emptyFormat = new Format(0<firstWidth>, 0<totalWidth>, 0<lastWidth>, 0<height>, fun _ _ -> "")

///Making Format from string.
let stringToFormat (s : string) = 
    let length = s.Length
    new Format(length * 1<firstWidth>, length * 1<totalWidth>, length * 1<lastWidth>, 1<height>, (fun _ p -> s + p))

///Adding indent to given Format.
let indentFormat (h : int) (format : Format) = 
    new Format(h * 1<firstWidth> + frameFirst format.widthInfo, h * 1<totalWidth> + frameMid format.widthInfo, h * 1<lastWidth> + frameLast format.widthInfo, format.height, 
               fun n s -> (spaces h) + format.txtstr (h + n) s)
               