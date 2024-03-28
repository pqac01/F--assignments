// #1
let squareList (inputList: int list) =
    List.map (fun x -> x * x) inputList

// #2
let filterOutOdds (inputList: int list) =
    List.filter (fun x -> x % 2 = 0) inputList

// #3
let sumOfPositives (inputList: int list) =
    List.sum (List.filter (fun x -> x > 0) inputList)

// #4
let capitalizeNames (names: string list) =
    List.map (fun (name: string) -> name.ToUpper()) names

// #5
let filterStringsByLength (strings: string list) (n: int) =
    List.filter (fun str -> String.length str > n) strings

// #6
let countDivisibleBy (numbers: int list) (divisor: int) =
    List.filter (fun num -> num % divisor = 0) numbers |> List.length

// #7
let findIndices (list: 'a list) (element: 'a) =
    list 
    |> List.mapi (fun index x -> if x = element then Some index else None)
    |> List.choose id

// #8
let concatenateLongStrings (strings: string list) (n: int) =
    strings 
    |> List.filter (fun str -> String.length str > n)
    |> String.concat ""

// #9
let findMaxTuple (tuples: ('id * 'value) list) =
    match tuples with
    | [] -> None
    | (id, value) :: rest ->
        let maxTuple (maxId, maxValue) (curId, curValue) =
            if curValue > maxValue then (curId, curValue) else (maxId, maxValue)
        
        Some (List.fold maxTuple (id, value) rest)

// #10
let countOccurrences (list: 'a list) =
    list
    |> List.groupBy id
    |> List.map (fun (element, occurrences) -> (element, List.length occurrences))

// #11
type TrafficLight =
    | Red
    | Yellow
    | Green

let nextState (currentState: TrafficLight) =
    match currentState with
    | Red -> Green
    | Yellow -> Red
    | Green -> Yellow

// #12
type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide

let performOperation (operation: ArithmeticOperation) (x: float) (y: float) =
    match operation with
    | Add -> x + y
    | Subtract -> x - y
    | Multiply -> x * y
    | Divide -> x / y

// #13
type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Square of sideLength: float

let calculateArea (shape: Shape) =
    match shape with
    | Circle radius -> 3.14 * radius * radius
    | Rectangle (width, height) -> width * height
    | Square sideLength -> sideLength * sideLength

// #14
type TemperatureScale =
    | Celsius
    | Fahrenheit

let convertTemperature (temperature: float) (fromScale: TemperatureScale) (toScale: TemperatureScale) =
    match fromScale, toScale with
    | Celsius, Fahrenheit -> (temperature * 9.0 / 5.0) + 32.0
    | Fahrenheit, Celsius -> (temperature - 32.0) * 5.0 / 9.0
    | _ -> temperature 

// #15
type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string
    | JsonNumber of float
    | JsonBoolean of bool

let rec prettyPrintJsonValue (jsonValue: JsonValue) =
    let indent = "  "

    let rec prettyPrintArray (jsonArray: JsonValue list) =
        let elements =
            jsonArray
            |> List.map (fun value -> prettyPrintJsonValue value)
            |> String.concat (sprintf ",\n%s" indent)
        sprintf "[\n%s%s\n]" indent elements

    let rec prettyPrintObject (jsonObject: (string * JsonValue) list) =
        let elements =
            jsonObject
            |> List.map (fun (key, value) -> sprintf "\"%s\": %s" key (prettyPrintJsonValue value))
            |> String.concat (sprintf ",\n%s" indent)
        sprintf "{\n%s%s\n}" indent elements

    match jsonValue with
    | JsonObject obj -> prettyPrintObject obj
    | JsonArray arr -> prettyPrintArray arr
    | JsonString str -> sprintf "\"%s\"" str
    | JsonNumber num -> string num
    | JsonBoolean bool -> string bool

// #16
let rec fibonacci (n: int) =
    if n <= 1 then
        n
    else
        fibonacci (n - 1) + fibonacci (n - 2)

// #17
let rec binarySearch (arr: int[]) (left: int) (right: int) (target: int) =
    if left > right then
        None 
    else
        let mid = (left + right) / 2
        if arr.[mid] = target then
            Some mid 
        elif arr.[mid] > target then
            binarySearch arr left (mid - 1) target 
        else
            binarySearch arr (mid + 1) right target 

let search (arr: int[]) (target: int) =
    binarySearch arr 0 (Array.length arr - 1) target

// #18
let rec mergeSort (list: int list) =
    let rec merge (left: int list) (right: int list) =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x <= y then
                x :: merge xs (y::ys)
            else
                y :: merge (x::xs) ys

    let rec split (lst: int list) =
        let rec splitAux (lst: int list) (acc1: int list) (acc2: int list) =
            match lst with
            | [] -> acc1, acc2
            | x::[] -> acc1 @ [x], acc2
            | x::y::tail -> splitAux tail (acc1 @ [x]) (acc2 @ [y])
        splitAux lst [] []

    match list with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let left, right = split list
        let sortedLeft = mergeSort left
        let sortedRight = mergeSort right
        merge sortedLeft sortedRight

// #19
type BinaryTree =
    | Node of value: int * left: BinaryTree * right: BinaryTree
    | Leaf

let rec treeDepth (tree: BinaryTree) =
    match tree with
    | Leaf -> 0
    | Node (_, left, right) ->
        let leftDepth = treeDepth left
        let rightDepth = treeDepth right
        1 + max leftDepth rightDepth

// 20
let rec isPalindrome (str: string) =
    let len = String.length str
    if len <= 1 then
        true 
    else
        let firstChar = str.[0]
        let lastChar = str.[len - 1]
        if firstChar = lastChar then
            isPalindrome (str.Substring(1, len - 2))
        else
            false