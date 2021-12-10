namespace Adventofcode2021

module Day9 =

    [<Literal>]
    let InputFile = "Day9Input.txt"

    let countLows (heigthmap: int [] []) =
        let maxX = Array.length heigthmap.[0] - 1
        let maxY = Array.length heigthmap - 1

        let riskLevel (heigthmap: int [] []) (x, y) =
            let upper =
                if y > 0 then
                    heigthmap.[y - 1].[x]
                else
                    System.Int32.MaxValue

            let lower =
                if y < maxY then
                    heigthmap.[y + 1].[x]
                else
                    System.Int32.MaxValue

            let left =
                if x > 0 then
                    heigthmap.[y].[x - 1]
                else
                    System.Int32.MaxValue

            let right =
                if x < maxX then
                    heigthmap.[y].[x + 1]
                else
                    System.Int32.MaxValue

            let p = heigthmap.[y].[x]

            if p < upper && p < lower && p < left && p < right then
                1 + p
            else
                0

        seq {
            for x in 0..maxX do
                for y in 0..maxY do
                    riskLevel heigthmap (x, y)
        }
        |> Seq.sum

    let day9 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.map (Array.map (string >> int))
        |> countLows
