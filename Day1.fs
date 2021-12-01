namespace Adventofcode2021

module Day1 =

    [<Literal>]
    let InputFile = "Day1Input.txt"

    let getMeasurements file =
        file
        |> System.IO.File.ReadAllLines
        |> Array.map int
        |> List.ofArray

    let countIncs f measurements =
        let rec helper sum m =
            match m with
            | x :: y :: rest ->
                let sum' = if (f x) < (f y) then sum + 1 else sum
                helper sum' (y :: rest)
            | _ -> sum

        helper 0 measurements

    let day1 () =
        InputFile |> getMeasurements |> countIncs id

    let day1Part2 () =
        InputFile
        |> getMeasurements
        |> List.windowed 3
        |> countIncs List.sum
