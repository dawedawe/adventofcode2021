namespace Adventofcode2021

module Day10 =

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
        let stack = System.Collections.Generic.Stack<char>()

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
