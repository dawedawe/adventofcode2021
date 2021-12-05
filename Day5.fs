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

    let points { Start = x1, y1; End = x2, y2 } =
        seq {
            let xs =
                if x1 = x2 then
                    List.replicate (abs (y1 - y2) + 1) x1
                else
                    let xInc = if x1 < x2 then 1 else - 1
                    [ x1..xInc..x2 ]

            let ys =
                if y1 = y2 then
                    List.replicate (abs (x1 - x2) + 1) y1
                else
                    let yInc = if y1 < y2 then 1 else - 1
                    [ y1..yInc..y2 ]

            for x, y in List.zip xs ys do
                x, y
        }

    let countOverlapPoints lines =
        let coordinates = Dictionary<int * int, int>()

        let rec helper lines =
            match lines with
            | [||] -> ()
            | _ ->
                lines.[0]
                |> points
                |> Seq.iter (fun p ->
                    if coordinates.ContainsKey(p) then
                        coordinates.[p] <- coordinates.[p] + 1
                    else
                        coordinates.[p] <- 1)

                helper (Array.tail lines)

        helper lines |> ignore
        coordinates.Values.Where(fun x -> x > 1).Count()

    let day5 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> Array.filter isHorizontalOrVertical
        |> countOverlapPoints

    let day5Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> countOverlapPoints
