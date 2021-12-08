namespace Adventofcode2021

module Day8 =

    [<Literal>]
    let InputFile = "Day8Input.txt"

    let parse (s: string) = s.Split(' ')[11..14]

    let count (outputs: string []) =
        outputs
        |> Array.sumBy (fun o ->
            match o.Length with
            | 2 | 3 | 4 | 7 -> 1
            | _ -> 0
            )

    let day8 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.map count
        |> Array.sum
