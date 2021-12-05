namespace Adventofcode2021

module Day5 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day5Input.txt"

    type Line = { Start: int * int; End: int * int }

    let parseLine (s: string) =
        let regex = System.Text.RegularExpressions.Regex(@"(\d+),(\d+) -> (\d+),(\d+)")
        let matches = regex.Matches s

        { Start = int matches.[0].Groups.[1].Value, int matches.[0].Groups.[2].Value
          End = int matches.[0].Groups.[3].Value, int matches.[0].Groups.[4].Value }

    let isHorizontalOrVertical { Start = x1, y1; End = x2, y2 } = x1 = x2 || y1 = y2

    let countOverlapPoints lines =
        let coordinates = Dictionary<int * int, int>()

        let rec helper lines =
            match lines with
            | [||] -> coordinates
            | _ ->
                let { Start = x1, y1; End = x2, y2 } = lines.[0]

                for x in (min x1 x2) .. (max x1 x2) do
                    for y in (min y1 y2) .. (max y1 y2) do
                        if coordinates.ContainsKey(x, y) then
                            coordinates.[(x, y)] <- coordinates.[(x, y)] + 1
                        else
                            coordinates.[(x, y)] <- 1

                helper (Array.tail lines)

        helper lines |> ignore
        coordinates.Values.Where(fun x -> x > 1).Count()

    let day5 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> Array.filter isHorizontalOrVertical
        |> countOverlapPoints
