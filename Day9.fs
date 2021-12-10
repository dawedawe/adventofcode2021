namespace Adventofcode2021

module Day9 =

    [<Literal>]
    let InputFile = "Day9Input.txt"

    let isLow maxX maxY (heigthmap: int [] []) (x, y) =
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

        p < upper && p < lower && p < left && p < right

    let countLows (heigthmap: int [] []) =
        let maxX = Array.length heigthmap.[0] - 1
        let maxY = Array.length heigthmap - 1

        seq {
            for x in 0..maxX do
                for y in 0..maxY do
                    if isLow maxX maxY heigthmap (x, y) then
                        1 + heigthmap.[y].[x]
        }
        |> Seq.sum


    let day9 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.map (Array.map (string >> int))
        |> countLows

    let getHigherNeighbours (heigthmap: int [] []) maxX maxY (x, y) =
        seq {
            if y > 0 && heigthmap.[y - 1].[x] < 9 then
                (x, y - 1)

            if y < maxY && heigthmap.[y + 1].[x] < 9 then
                (x, y + 1)

            if x > 0 && heigthmap.[y].[x - 1] < 9 then
                (x - 1, y)

            if x < maxX && heigthmap.[y].[x + 1] < 9 then
                (x + 1, y)
        }

    let bfs heigthmap maxX maxY pos =
        let mutable bazin = Set.singleton pos

        let queue = System.Collections.Generic.Queue<int * int>()

        let higherNeighbours = getHigherNeighbours heigthmap maxX maxY pos

        for n in higherNeighbours do
            queue.Enqueue n

        while (queue.Count <> 0) do
            let currentPos = queue.Dequeue()

            let higherNeighbours =
                getHigherNeighbours heigthmap maxX maxY currentPos
                |> Seq.filter (fun n -> not (Set.contains n bazin))

            for n in higherNeighbours do
                bazin <- Set.add n bazin
                queue.Enqueue n

        bazin.Count


    let bazinSizes (heigthmap: int [] []) =
        let maxX = Array.length heigthmap.[0] - 1
        let maxY = Array.length heigthmap - 1

        seq {
            for x in 0..maxX do
                for y in 0..maxY do
                    if isLow maxX maxY heigthmap (x, y) then
                        bfs heigthmap maxX maxY (x, y)
        }

    let day9Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.map (Array.map (string >> int))
        |> bazinSizes
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.reduce (*)
