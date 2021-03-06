namespace Adventofcode2021

module Day15 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day15Input.txt"

    let getNeighbours (cave: int [] []) (x, y) =
        let maxX = cave.[0].Length - 1
        let maxY = cave.Length - 1
        let upper = x, y - 1
        let lower = x, y + 1
        let left = x - 1, y
        let right = x + 1, y

        [ left; right; upper; lower ]
        |> List.filter (fun (x, y) -> 0 <= x && x <= maxX && 0 <= y && y <= maxY)

    let length (cave: int [] []) u (vx, vy) = cave.[vy].[vx]

    let dijkstra target (cave: int [] []) =
        let q = HashSet<(int * int)>()
        let dist = Dictionary<(int * int), int>()
        let prev = Dictionary<(int * int), (int * int) option>()

        let maxX = cave.[0].Length - 1
        let maxY = cave.Length - 1

        for y in 0..maxY do
            for x in 0..maxX do
                dist.[(x, y)] <- System.Int32.MaxValue
                prev.[(x, y)] <- None
                q.Add((x, y)) |> ignore

        dist.[(0, 0)] <- 0

        while q.Count > 0 do
            let u = q.MinBy(fun v -> dist.[v])
            q.Remove(u) |> ignore

            let neighbours =
                getNeighbours cave u
                |> List.filter (fun v -> q.Contains(v))

            for v in neighbours do
                let alt = dist.[u] + length cave u v

                if alt < dist.[v] then
                    dist.[v] <- alt
                    prev.[v] <- Some u

        dist, prev

    let day15 () =
        let cave =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map (fun s -> s.ToCharArray())
            |> Array.map (fun a -> a |> Array.map (string >> int))

        let target = cave.[0].Length - 1, cave.Length - 1
        let dist, prev = dijkstra target cave
        dist[target]

    let raiseRisk xDir yDir risk =
        let risk' = risk + xDir + yDir
        if risk' > 9 then risk' - 9 else risk'

    let copyCave (cave: int [] []) =
        [| for y in 0 .. cave.Length - 1 do
               Array.copy cave.[y] |]

    let repeatCave (cave: int [] []) xDir yDir =
        let maxX = cave.[0].Length - 1
        let maxY = cave.Length - 1
        let cave' = copyCave cave

        for y in 0..maxY do
            for x in 0..maxX do
                cave'.[y].[x] <- raiseRisk xDir yDir cave.[y].[x]

        cave'

    let day15Part2 () =
        let cave =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map (fun s -> s.ToCharArray())
            |> Array.map (fun a -> a |> Array.map (string >> int))

        let mutable entireCave = copyCave cave

        for x in 1..4 do
            let cave' = repeatCave cave x 0

            for y in 0 .. cave.Length - 1 do
                entireCave.[y] <- Array.append entireCave.[y] cave'.[y]

        let copy = copyCave entireCave

        for y in 1..4 do
            let cave' = repeatCave copy 0 y
            entireCave <- Array.append entireCave cave'

        let target = entireCave.[0].Length - 1, entireCave.Length - 1
        let dist, prev = dijkstra target entireCave
        dist[target]
