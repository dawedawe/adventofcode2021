namespace Adventofcode2021

module Day12 =

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type Connection = { Cave1: string; Cave2: string }

    let parse (s: string) =
        let parts = s.Split('-')
        { Cave1 = parts.[0]; Cave2 = parts.[1] }

    let isSmallCave (s: string) =
        s.ToCharArray()
        |> Array.forall System.Char.IsLower

    let getNextPaths (connections: Connection []) path =
        let pos = List.last path
        let visitedSmallCaves = path |> List.filter isSmallCave

        let neighbours =
            connections
            |> Array.filter (fun c -> c.Cave1 = pos || c.Cave2 = pos)
            |> Array.map (fun c ->
                if c.Cave1 = pos then
                    c.Cave2
                else
                    c.Cave1)
            |> Array.filter (fun c ->
                match c with
                | _ when isSmallCave c -> visitedSmallCaves |> List.contains c |> not
                | _ -> true)

        neighbours
        |> Array.map (fun n -> List.append path [ n ])

    let bfs getNextPathsF (connections: Connection []) =
        let mutable paths = Set.empty

        let queue = System.Collections.Generic.Queue<string list>()

        getNextPathsF connections [ "start" ]
        |> Array.iter (fun p -> queue.Enqueue p)

        while (queue.Count <> 0) do
            let currentPath = queue.Dequeue()

            if (List.last currentPath) = "end" then
                paths <- Set.add currentPath paths
            else
                getNextPathsF connections currentPath
                |> Array.iter (fun p -> queue.Enqueue p)

        paths.Count

    let day12 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> bfs getNextPaths

    let getNextPathsPart2 (connections: Connection []) path =
        let pos = List.last path
        let visitedSmallCaves = path |> List.filter isSmallCave
        let smallCaveVisits = visitedSmallCaves |> List.countBy id |> Map.ofList

        let smallCaveVisitedTwice =
            visitedSmallCaves
            |> Set.ofList
            |> fun s -> s.Count < visitedSmallCaves.Length

        let neighbours =
            connections
            |> Array.filter (fun c -> c.Cave1 = pos || c.Cave2 = pos)
            |> Array.map (fun c ->
                if c.Cave1 = pos then
                    c.Cave2
                else
                    c.Cave1)
            |> Array.filter (fun c ->
                match c with
                | "start" -> false
                | _ when isSmallCave c ->
                    let alreadyVisited = visitedSmallCaves |> List.contains c

                    if alreadyVisited then
                        smallCaveVisits.[c] < 2
                        && not smallCaveVisitedTwice
                    else
                        true
                | _ -> true)

        neighbours
        |> Array.map (fun n -> List.append path [ n ])

    let day12Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> bfs getNextPathsPart2
