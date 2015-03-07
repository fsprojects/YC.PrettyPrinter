module Format

open System.Collections.Generic

let spaces n = 
    let mutable spaces = ""
    for i in 0..n-1 do
        spaces <- spaces +  " "
    spaces

type T_Height  = int
type T_LastWidht = int
type T_TotalWidht  = int
//Basic class with some fields
//Height - total height of our box
//widthLast - width of last line
//width -  total width of our box
type Format =
    val height : T_Height
    val widthLast : T_LastWidht
    val width: T_TotalWidht
    val txtstr : int -> string -> string
    
    new(height0, widthLast0, width0, txtstr0) = {
        height = height0;
        widthLast = widthLast0;
        width = width0;
        txtstr = txtstr0;
        }
    static member (==) (format1 : Format, format2 : Format) =
        format1.height = format2.height 
        && format1.width = format2.width
        && format1.widthLast = format2.widthLast

//Above Format
    static member (>-<) (f1 : Format, f2 : Format) = 
        let makeIndentsAbove =
            fun n _ -> ("\n" + spaces n) + f2.txtstr n "" |> f1.txtstr n
        new Format(f1.height+f2.height, f2.widthLast, max f1.width f2.width, makeIndentsAbove)

//Beside Format !!!!WARNING "" is wery stange no spaces
    static member (>|<) (f1 : Format, f2 : Format) =
        new Format(f1.height+f2.height-1, 
                   f1.widthLast+f2.widthLast, 
                   max f1.width (f1.widthLast+f2.width), 
                   fun n _ -> "" |> (f2.txtstr (f2.widthLast + n) >> f1.txtstr n) )

//Is that correctly???
    member this.isSuitable width =
        this.width <= width

//Comparable realisation
    interface System.IComparable with
      member this.CompareTo format2 =
        match format2 with
          | :? Format as f2 -> 
            if this.height < f2.height 
            then -1
            elif (this.height = f2.height)
            then if this.width < f2.width
                 then -1
                 else 0
            else 1
          | _ -> invalidArg "format2" "cannot compare values of different types"

//Makin Format from string
let stringToFormat (s:string) =
    let length = s.Length;
    new Format(length, length, length, (fun _ p -> s + p ))

//Adding indent to given Format 
let indentFormat h (format:Format) =
    new Format(format.height, h+format.widthLast, h+format.width, fun n s-> format.txtstr (h + n) (spaces h) + s)

type Frame =
    val width : int
    val widthLast : int

    new(width0, widthLast0) = { width = width0; widthLast = widthLast0;}
    
    //Comparable realisation
    interface System.IComparable with
      member this.CompareTo frame2 =
        match frame2 with
          | :? Frame as f2 -> 
            if this.width < f2.width 
            then -1
            elif (this.width = f2.width)
            then if this.widthLast < f2.widthLast
                 then -1
                 else 0
            else 1
          | _ -> invalidArg "format2" "cannot compare values of different types"
    
    member this.isSuitable (width:int) =
        this.width <= width
   
let formatToFrame (format:Format) =
    new Frame(format.width, format.widthLast)
