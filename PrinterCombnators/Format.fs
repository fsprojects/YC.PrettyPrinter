module Format

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

type Format = 
    val height : T_Height
    val widthLast : T_LastWidht
    val width : T_TotalWidht
    val widthFirst : T_FirstWidth
    val txtstr : int -> string -> string
    
    new(firstWidth0, width0, widthLast0, height0, txtstr0) = 
        { height = height0
          widthLast = widthLast0
          width = width0
          txtstr = txtstr0
          widthFirst = firstWidth0 }
    
    static member (==) (format1 : Format, format2 : Format) = 
        format1.height = format2.height && format1.width = format2.width && format1.widthLast = format2.widthLast
    
    ///Returns if format.width < width
    member this.isSuitable width = this.width <= width
    
    //Frame3d
    member this.ToFrame = new Frame(this.widthFirst, this.width, this.widthLast)
    member this.toString = this.txtstr 0 ""
    
    ///Above Format
    static member (>-<) (f1 : Format, f2 : Format) = 
        let makeIndentsAbove = fun n -> f1.txtstr n << nl_skip n << f2.txtstr n
        let newFirst = f1.widthFirst
        
        let newMid = 
            if f1.height > 1 then max f1.width f1.widthLast
            elif f2.height > 1 then max f2.widthFirst f2.width
            else max f1.width f1.widthLast
        
        let newLast = 
            if f2.widthLast <> 0 then f2.widthLast
            else f1.widthLast
        
        let newHeight = f1.height + f2.height
        new Format(newFirst, newMid, newLast, newHeight, makeIndentsAbove)
    
    ///Beside Format
    static member (>|<) (f1 : Format, f2 : Format) = 
        let newFirst = 
            (if f1.height <> 1 then f1.widthFirst
             else f1.widthFirst + f2.widthFirst)
        
        let newMid = 
            if f1.height > 1 then f1.width
            elif f1.height = 1 && f2.height = 1 then f1.width + f2.width
            elif f2.height > 1 then max (f1.widthLast + f2.widthFirst) (f1.widthLast + f2.width)
            else f1.width
        
        let newLast = f1.widthLast + f2.widthLast
        let newHeight = f1.height + f2.height - 1
        let newFun = fun n -> f1.txtstr n << f2.txtstr (f1.widthLast + n)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    ///Fill format
    static member (>/<) (f1 : Format, f2 : Format) = Format.addFill (f1, f2, 1)
    
    static member addFill (f1 : Format, f2 : Format, shift : int) = 
        let newFirst = 
            if f1.height <> 1 then f1.widthFirst
            else f1.widthFirst + f2.widthFirst
        
        let newMid = 
            if f1.height = 1 && f2.height = 1 then f1.width + f2.width
            elif f1.height > 1 && f2.height = 1 then f1.width
            elif f1.height = 1 && f2.height > 1 then f2.width + shift
            elif f1.height > 1 && f2.height > 1 then max (f1.widthLast + f2.widthFirst) (f2.width + shift)
            else 0
        
        let newLast = 
            if f2.height <> 1 then f2.widthLast + shift
            else f1.widthLast + f2.widthLast
        
        let newHeight = f1.height + f2.height - 1
        let newFun = fun n -> f1.txtstr n << f2.txtstr (n + shift)
        new Format(newFirst, newMid, newLast, newHeight, newFun)
    
    //Format comparable realisation      
    interface System.IComparable with
        member this.CompareTo format2 = 
            match format2 with
            | :? Format as f2 -> 
                if (this.height < f2.height) || (this.height = f2.height && this.width < f2.width) then -1
                elif (this.height = f2.height && this.width = f2.width && this.widthLast = this.widthLast) then 0
                else 1
            | _ -> invalidArg "format2" "cannot compare values of different types"

let emptyFormat = new Format(0, 0, 0, 0, fun _ _ -> "")

//Makin Format from string
let stringToFormat (s : string) = 
    let length = s.Length
    new Format(length, length, length, 1, (fun _ p -> s + p))

//Adding indent to given Format 
let indentFormat h (format : Format) = 
    new Format(h + format.widthFirst, h + format.width, h + format.widthLast, format.height, 
               fun n s -> (spaces h) + format.txtstr (h + n) s)
