//
// F#-based PPM image library.
//
// <<JAMES KLONOWSKI>>
// U. of Illinois, Chicago
// CS341, Spring 2015
// Homework 6
//

module PPMImageLibrary
#light

// DebugOutput:
// Outputs to console, which appears in the "Output" window pane of Visual Studio when you run with debugging (F5).
let rec private OutputImage(image:int list list) = 
  if image = [] then
    printfn "**END**"
  else
    printfn "%A" image.Head
    OutputImage(image.Tail)
           

let DebugOutput(width:int, height:int, depth:int, image:int list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage(image)


// An example transformation: replaces the first row of the given image with a row of all white pixels.
let rec BuildRowOfWhite cols white = 
  if cols = 0 then []
  else 
    white :: white :: white :: BuildRowOfWhite (cols-1) white

let TransformFirstRowWhite(width:int, height:int, depth:int, image:int list list) = 
  BuildRowOfWhite width depth :: image.Tail


//Given an input string and a string of characters, return the original string with those character removed
let stripChars text (chars:string) =
  Array.fold(fun (s:string) c->s.Replace(c.ToString()," ")) text (chars.ToCharArray())


//given a string list, remove unwanted characters
let rec FormatP3 L:string list = 
  if L = [] then []
  else
    (stripChars L.Head "[;]") :: FormatP3 L.Tail


//convert list to string list
let convertToString L = 
  L |> List.map (fun i -> i.ToString()) |> String.concat " "


// Writes the given image out to a text file, in "P3" format.  Returns true if successful, false if not.
let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:int list list) = 
  let dimensions = String.concat " " [string width; string height]
  let img = FormatP3 (List.map convertToString image)
  let L = "P3" :: dimensions :: (string depth) :: img
  System.IO.File.WriteAllLines(filepath, L)
  true  // success


//given an int list and number of columns, return an int list of the averages of groups of three
let rec averageRow (L:int list) cols = 
  if cols = 0 then []
  else
    //let avg = ((List.nth L 0)+(List.nth L 1)+(List.nth L 2))/3
    let avg = (L.Head+L.Tail.Head+L.Tail.Tail.Head)/3
    avg :: avg :: avg :: (averageRow L.Tail.Tail.Tail (cols-1))


//recursively transform an image of the form int list list into grayscale
let rec Grayscale (L:int list list) (cols:int)=
  if L = [] then []
  else
    (averageRow L.Head cols) :: (Grayscale L.Tail cols)
    
  
//invert a single row
let rec invertRow (L:int list) depth =
  if L = [] then []
  else
    let first = depth - L.Head
    first :: invertRow L.Tail depth


//invert an image
let rec invert (L:int list list) depth =
  if L = [] then []
  else
    (invertRow L.Head depth) :: (invert L.Tail depth)
    

//Reverse every three pixels
let rec FlipThree (L:int list) cols =
  if cols = 0 then []
  else if L = [] then []
  else
    let first = L.Tail.Tail.Head
    let second = L.Tail.Head
    let third = L.Head
    first :: second :: third :: (FlipThree L.Tail.Tail.Tail (cols-1))

//Given an int list list and the width, flips the image horizontally
let rec HFlip (L:int list list) cols =
  if L = [] then []
  else
    (List.rev (FlipThree L.Head cols)) :: (HFlip L.Tail cols)


//Given an int list list, returns the first half of the list
let rec getHalf (L:int list list) halfHeight = 
  if halfHeight = 0 then []
  else
    L.Head :: (getHalf L.Tail  (halfHeight-1))


//function that creates a column list from a row list
let rec rowToCol transposedList (L:((int*int*int) list list)) =
  if L.Head.Length = 0 then (List.rev transposedList)
  else
    //get the first tuple from each row and create a new list
    let firstItems = List.map List.head L
    //make a new tuple list list without the head of each row
    let restOfList = List.map (fun row->match row with []->[] | head::tail -> tail) L
    //bazinga! this took forever to figure out...
    rowToCol (firstItems::transposedList) restOfList


//helper function to transpose a tuple list list
let transpose (L:(int*int*int) list list) =
  rowToCol [] L

//reverse the rows of a list of lists
let rec reverseRows L = 
  if L = [] then []
  else
    (L.Head|>List.rev) :: (reverseRows L.Tail)
  
  
//Read in an int list, and return a list of int tuples of the form (x,y,z)
let rec makeTuples (L:int list) =
  if L = [] then []
  else
    (L.Head, L.Tail.Head, L.Tail.Tail.Head) :: (makeTuples L.Tail.Tail.Tail)

//transform a list of tuples into an int list
let rec tuplesToList (L:(int*int*int) list) =
  if L = [] then []
  else
    let x,y,z = L.Head
    x::y::z::tuplesToList L.Tail

//Grayscale master function
let TransformGrayscale(width:int, height:int, depth:int, image:int list list) =   
  Grayscale image width

//Invert master function
let TransformInvert(width:int, height:int, depth:int, image:int list list) = 
  invert image depth

//Horizontal Flip master function
let TransformFlipHorizontal(width:int, height:int, depth:int, image:int list list) = 
  HFlip image width

//Vertical Flip master function
let TransformFlipVertical(width:int, height:int, depth:int, image:int list list) = 
  let firstHalfReversed = List.rev (getHalf image (height/2))
  let secondHalfReversed = (getHalf (List.rev image) (height/2))
  secondHalfReversed @ firstHalfReversed

//Rotate master function
let RotateRight90(width:int, height:int, depth:int, image:int list list) = 
  let tuplesList = image|>List.map makeTuples
  let transposition = transpose tuplesList
  let rotatedTuples = reverseRows transposition
  rotatedTuples|>List.map tuplesToList