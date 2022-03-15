namespace Adventofcode2021

module Day18 =

    open System
    open System.Collections.Generic

    [<Literal>]
    let InputFile = "Day18Input.txt"

    let getNextPair (s: string) =
        s.ToCharArray()
        |> Array.takeWhile (fun c -> c <> ']')
        |> String
        |> fun s -> s + "]"

    let (|OpenBrack|CloseBrack|Digit|Comma|Empty|) (s: string) =
        match s with
        | "" -> Empty
        | _ when s.[0] = '[' -> OpenBrack
        | _ when s.[0] = ']' -> CloseBrack
        | _ when Char.IsDigit s.[0] -> Digit
        | _ when s.[0] = ',' -> Comma
        | _ -> failwith $"unsupported token {s}"

    let getPair (n: string) =
        let openStack = Stack<string>()
        let closeStack = Stack<string>()

        let rec helper idx =
            match n.[idx..] with
            | Empty -> failwith "no pair found"
            | OpenBrack ->
                openStack.Push("[")
                helper (idx + 1)
            | CloseBrack ->
                closeStack.Push("]") |> ignore
                helper (idx + 1)
            | Digit -> helper (idx + 1)
            | Comma ->
                if openStack.Count = closeStack.Count + 1 then
                    let left = n.[1 .. idx - 1]
                    let right = n.[idx + 1 .. n.Length - 2]
                    (left, right)
                else
                    helper (idx + 1)

        helper 0

    let getLeftRight (pair: string) =
        pair.Trim('[', ']')
        |> fun s -> s.Split(',')
        |> fun a -> int a.[0], int a.[1]

    let findPairToExplode (stack: Stack<string>) (n: string) =
        let rec helper idx =
            match n.[idx..] with
            | Empty -> None
            | OpenBrack ->
                stack.Push("[")

                if stack.Count = 5 then
                    Some idx
                else
                    helper (idx + 1)
            | CloseBrack ->
                stack.Pop() |> ignore
                helper (idx + 1)
            | Digit
            | Comma -> helper (idx + 1)

        helper 0

    let digits =
        [| '0'
           '1'
           '2'
           '3'
           '4'
           '5'
           '6'
           '7'
           '8'
           '9' |]

    let addToFirstLeft (n: string) toAdd =
        let index = n.LastIndexOfAny(digits)

        if index >= 0 then
            let (index', length) =
                if Char.IsDigit(n.[index - 1]) then
                    index - 1, 2
                else
                    index, 1

            let left = n.Substring(index', length) |> int
            let sum = left + toAdd

            n.Substring(0, index')
            + sum.ToString()
            + n.Substring(index + 1)
        else
            n

    let addToFirstRight (n: string) toAdd =
        let index = n.IndexOfAny(digits)

        if index >= 0 then
            let length =
                if Char.IsDigit(n.[index + 1]) then
                    2
                else
                    1

            let right = n.Substring(index, length) |> int
            let sum = right + toAdd

            n.Substring(0, index)
            + sum.ToString()
            + n.Substring(index + length)
        else
            n

    let explode (n: string) (idx: int) =
        let toExp = getNextPair n.[idx..]
        let left, right = getLeftRight toExp
        let leftPart = addToFirstLeft (n.Substring(0, idx)) left
        let rightPart = addToFirstRight (n.Substring(idx + toExp.Length)) right

        let nAfterReplace = leftPart + "0" + rightPart

        nAfterReplace

    let tryExplode (n: string) =
        n
        |> findPairToExplode (Stack<string>())
        |> function
            | Some idx -> explode n idx
            | None -> n

    let split n =
        let left = n / 2
        let right = float n / 2. |> ceil |> int
        $"[{left},{right}]"

    let trySplit (n: string) =
        n.ToCharArray()
        |> Array.windowed 2
        |> Array.tryFindIndex (fun w ->
            match w with
            | [| x; y |] -> Char.IsDigit x && Char.IsDigit y
            | _ -> false)
        |> function
            | Some index ->
                let prefix = n.Substring(0, index)
                let splited = split (n.Substring(index, 2) |> int)
                let postfix = n.Substring(index + 2)
                prefix + splited + postfix
            | None -> n

    let rec reduce (n: string) =
        let afterExp = tryExplode n

        if afterExp <> n then
            reduce afterExp
        else
            let afterSplit = trySplit n

            if afterSplit <> n then
                reduce afterSplit
            else
                n

    let add n1 n2 = $"[{n1},{n2}]" |> reduce

    let rec magnitude (n: string) =
        let left, right = getPair n

        let magLeft =
            if Char.IsDigit left[0] then
                3 * int (left[ 0 ].ToString())
            else
                3 * magnitude left

        let magRight =
            if Char.IsDigit right[0] then
                2 * int (right[ 0 ].ToString())
            else
                2 * magnitude right

        magLeft + magRight

    let day18 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.reduce add
        |> magnitude

    let day18Part2 () =
        let numbers = System.IO.File.ReadAllLines InputFile
        let indexes = [ 0 .. numbers.Length - 1 ]

        seq {
            for n in indexes do
                let rest = indexes |> List.filter (fun i -> i <> n)

                for r in rest do
                    (numbers[n], numbers[r])
                    (numbers[r], numbers[n])
        }
        |> Seq.map (fun (a, b) -> ((add a b) |> magnitude))
        |> Seq.max
