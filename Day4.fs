namespace Adventofcode2021

module Day4 =

    open System

    let InputFile = "Day4Input.txt"

    type Board = Map<int * int, int * bool>

    let parseBoard (lines: string []) =
        let mutable board = Map.empty

        for y in 0..4 do
            let lineNumbers =
                lines.[y].Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int

            for x in 0..4 do
                board <- Map.add (x, y) (lineNumbers[x], false) board

        board

    let parseInput (lines: string []) =
        let numbers =
            lines.[0].Split ','
            |> Array.map int
            |> List.ofArray

        let boards =
            [ for i in 1..6 .. lines.Length - 1 do
                  lines.[i + 1 .. i + 5] |> parseBoard ]

        numbers, boards

    let playBoard n (board: Board) =
        let key = board |> Map.tryFindKey (fun _ v -> (fst v) = n)

        match key with
        | Some k -> board |> Map.add k (n, true)
        | None -> board

    let boardWon (board: Board) =
        seq {
            for x in 0..4 do
                [ for y in 0..4 -> snd board.[x, y] ]
                |> List.forall id

            for y in 0..4 do
                [ for x in 0..4 -> snd board.[x, y] ]
                |> List.forall id
        }
        |> Seq.contains true

    let sumOfUnmarked (board: Board) =
        board.Values
        |> Seq.filter (fun (_, m) -> m = false)
        |> Seq.sumBy fst

    let rec playBoards (numbers, boards) =
        match numbers with
        | [] -> failwith "no numbers left"
        | n :: rest ->
            let boards' = boards |> List.map (playBoard n)
            let winningBoards = boards' |> List.filter boardWon

            match winningBoards.Length with
            | 0 -> playBoards (rest, boards')
            | 1 ->
                let s = sumOfUnmarked winningBoards.[0]
                s * n
            | _ -> failwith "multiple boards won"

    let day4 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> parseInput
        |> playBoards
