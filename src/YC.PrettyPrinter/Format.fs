module YC.PrettyPrinter.Format

//Bug when working with empty format, and 1 height format
//1 height fixed
open System.Collections.Generic

let spaces n = String.replicate n " "
let nl_skip n s = "\r\n" + spaces n + s

type T_Height = int

type T_LastWidht = int

type T_TotalWidht = int

type T_FirstWidth = int

//Basic class with some fields
//Height - total height of our box
//widthLast - width of last line
//width -  total width of our box
type Frame = 
    val first : int
    val mid : int
    val last : int
    
    new(first0, width0, widthLast0) = 
        { first = first0
          mid = width0
          last = widthLast0 }
    
    //Frame comparable realisation
    interface System.IComparable with
        member this.CompareTo frame2 = 
            match frame2 with
            | :? Frame as f2 -> 
                if this.first < f2.first && this.mid < f2.mid && this.last < f2.last then -1
                elif this.first = f2.first && this.mid = f2.mid && this.last = f2.last then 0
                else 1
            | _ -> invalidArg "format2" "cannot compare values of different types"
    
    override x.Equals(yobj) =
        match yobj with
        | :? Frame as f2 -> (x.first = f2.first && x.mid = f2.mid && x.last = f2.last)
        | _ -> false

    override x.GetHashCode() =
        hash (x.first, x.mid, x.last)

type Format = 
    val height : T_Height
    val last : T_LastWidht
    val mid : T_TotalWidht
    val first : T_FirstWidth
    val txtstr : int -> string -> string
    
    new(firstWidth0, width0, widthLast0, height0, txtstr0) = 
        { height = height0
          last = widthLast0
          mid = width0
          txtstr = txtstr0
          first = firstWidth0 }
    
    static member (==) (format1 : Format, format2 : Format) = 
        format1.height = format2.height && format1.mid = format2.mid && format1.last = format2.last
    
    ///Returns if format.width < width
    member this.isSuitable width = this.totalW <= width
    member this.totalW = List.max[this.first; this.mid; this.last]
    //Frame3d
    member this.ToFrame = new Frame(this.first, this.mid, this.last)
    member this.toString = this.txtstr 0 ""
    
    ///Above Format
    static member (>-<) (f1 : Format, f2 : Format) = 
        let newFirst = f1.first
        let newMid = 
            List.max [(if f1.height > 1 then max f1.mid f1.last  else 0); 
                      (if f2.height > 1 then max f2.first f2.mid else max f1.mid f1.last);]
        let newLast = 
            if f2.last <> 0 then f2.last
            else f1.last
        let newHeight = f1.height + f2.height
        let newFun = fun n -> f1.txtstr n << nl_skip n << f2.txtstr n
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Beside Format
    static member (>|<) (f1 : Format, f2 : Format) = 
        let newFirst = 
            (if f1.height <> 1 then f1.first
             else f1.first + f2.first)
        let newMid = 
            List.max [(if f1.height > 1 then f1.mid else 0); 
                      (if f1.height = 1 && f2.height=1 then f1.mid+f2.mid else 0);
                      (if f2.height > 1 then max (f1.last + f2.first) (f1.last + f2.mid) 
                                        else f1.mid);]
        let newLast = f1.last + f2.last
        let newHeight = f1.height + f2.height - 1
        let newFun = fun n -> f1.txtstr n << f2.txtstr (f1.last + n)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Fill format
    static member (>/<) (f1 : Format, f2 : Format) = Format.addFill (f1, f2, 1)
    
    static member addFill (f1 : Format, f2 : Format, shift : int) = 
        let newFirst = 
            if f1.height <> 1 then f1.first
            else f1.first + f2.first
        let newMid =
            List.max [(if f1.height = 1 && f2.height = 1 then f1.mid+f2.mid else 0);
                      (if f1.height > 1 && f2.height = 1 then f1.mid          else 0); 
                      (if f1.height = 1 && f2.height > 1 then f2.mid + shift  else 0);
                      (if f1.height > 1 && f2.height > 1 then max (f1.last + f2.first) (f2.mid + shift) 
                                                         else 0);]
        let newLast = 
            if f2.height <> 1 then f2.last + shift
            else f1.last + f2.last
        let newHeight = f1.height + f2.height - 1
        let newFun = fun n -> f1.txtstr n << f2.txtstr (n + shift)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    //Format comparable realisation      
    interface System.IComparable with
        member this.CompareTo format2 = 
            match format2 with
            | :? Format as f2 -> 
                if (this.height < f2.height) || (this.height = f2.height && this.totalW < f2.totalW) then -1 //(this.height = f2.height && this.width < f2.width) then -1
                elif (this.height = f2.height && this.mid = f2.mid && this.last = this.last) then 0
                else 1
            | _ -> invalidArg "format2" "cannot compare values of different types"

    override x.Equals(yobj) = 
        match yobj with
        | :? Format as y -> x.height = y.height && x.first = y.first && x.mid = y.mid && x.last = y.last
        | _ -> false
    
    override x.GetHashCode() =
        hash (x.height, x.first, x.mid, x.last)

let emptyFormat = new Format(0, 0, 0, 0, fun _ _ -> "")

//Makin Format from string
let stringToFormat (s : string) = 
    let length = s.Length
    new Format(length, length, length, 1, (fun _ p -> s + p))

//Adding indent to given Format 
let indentFormat h (format : Format) = 
    new Format(h + format.first, h + format.mid, h + format.last, format.height, 
               fun n s -> (spaces h) + format.txtstr (h + n) s)
