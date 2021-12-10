namespace Adventofcode2021

module Day10 =

    open System.Collections.Generic

    [<Literal>]
    let InputFile = "Day10Input.txt"

    let scoreCorrupted =
        function
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> failwith "unknown char"

    let isCorrupted (s: string) =
        let stack = Stack<char>()

        let rec helper a =
            if Array.isEmpty a then
                0
            else
                let c = Array.head a

                match c with
                | '('
                | '['
                | '{'
                | '<' ->
                    stack.Push c
                    helper (Array.tail a)
                | ')'
                | ']'
                | '}'
                | '>' ->
                    let opener = stack.Pop()

                    match opener, c with
                    | '(', ')'
                    | '[', ']'
                    | '{', '}'
                    | '<', '>' -> helper (Array.tail a)
                    | _ -> scoreCorrupted c
                | _ -> failwith "unknown char"

        s.ToCharArray() |> helper

    let day10 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map isCorrupted
        |> Array.sum

    let autoComplete (stack: Stack<char>) =
        let rec helper missing =
            if stack.Count = 0 then
                missing
            else
                let opener = stack.Pop()

                let m =
                    match opener with
                    | '(' -> ')'
                    | '[' -> ']'
                    | '{' -> '}'
                    | '<' -> '>'
                    | _ -> failwith "bad state"

                let missing' = Array.append missing [| m |]
                helper missing'

        helper Array.empty

    let complete (s: string) =
        let stack = Stack<char>()

        let rec helper a =
            if Array.isEmpty a then
                autoComplete stack
            else
                let c = Array.head a

                match c with
                | '('
                | '['
                | '{'
                | '<' ->
                    stack.Push c
                    helper (Array.tail a)
                | ')'
                | ']'
                | '}'
                | '>' ->
                    let opener = stack.Pop()

                    match opener, c with
                    | '(', ')'
                    | '[', ']'
                    | '{', '}'
                    | '<', '>' -> helper (Array.tail a)
                    | _ -> failwith "bad state"

                | _ -> failwith "unknown char"

        s.ToCharArray() |> helper

    let scoreCompletion a =
        let rec helper s a =
            if Array.isEmpty a then
                s
            else
                let c = Array.head a

                let points =
                    match c with
                    | ')' -> 1L
                    | ']' -> 2L
                    | '}' -> 3L
                    | '>' -> 4L
                    | _ -> failwith "bad state"

                let s' = 5L * s + points
                helper s' (Array.tail a)

        helper 0L a

    let day10Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.filter (fun s -> s |> isCorrupted = 0)
        |> Array.map complete
        |> Array.map scoreCompletion
        |> Array.sort
        |> fun a -> a |> Array.item (a.Length / 2)
