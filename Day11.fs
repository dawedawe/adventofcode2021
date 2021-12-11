namespace Adventofcode2021

module Day11 =

    [<Literal>]
    let InputFile = "Day11Input.txt"

    let incAll (octopuses: int [] []) =
        octopuses
        |> Array.map (fun line -> line |> Array.map (fun x -> x + 1))

    let getNeighbours (octopuses: int [] []) (x, y) =
        let maxX = octopuses.[0].Length - 1
        let maxY = octopuses.Length - 1

        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x + 1, y)
          (x + 1, y + 1)
          (x, y + 1)
          (x - 1, y + 1)
          (x - 1, y) ]
        |> List.filter (fun (x, y) -> 0 <= x && x <= maxX && 0 <= y && y <= maxY)

    let incNeighbours octopuses (x, y) =
        let neighbours = getNeighbours octopuses (x, y)

        for (x, y) in neighbours do
            octopuses.[y].[x] <- octopuses.[y].[x] + 1

        octopuses

    let rec incFlashNeighbours octopuses flashers =
        match flashers with
        | p :: rest ->
            let octopuses' = incNeighbours octopuses p
            incFlashNeighbours octopuses' rest
        | [] -> octopuses

    let rec flash (octopuses: int [] []) alreadyFlashed =
        let maxX = octopuses.[0].Length - 1
        let maxY = octopuses.Length - 1

        let flashers =
            seq {
                for y in 0..maxY do
                    for x in 0..maxX do
                        if
                            octopuses.[y].[x] >= 10
                            && not (List.contains (x, y) alreadyFlashed)
                        then
                            (x, y)
            }
            |> Seq.toList

        if Seq.isEmpty flashers then
            octopuses
        else
            let octopuses' = incFlashNeighbours octopuses flashers
            let alreadyFlashed' = List.append flashers alreadyFlashed
            flash octopuses' alreadyFlashed'

    let countFlashers (octopuses: int [] []) =
        let maxX = octopuses.[0].Length - 1
        let maxY = octopuses.Length - 1

        seq {
            for y in 0..maxY do
                for x in 0..maxX do
                    if octopuses.[y].[x] >= 10 then 1
        }
        |> Seq.sum

    let resetFlashers (octopuses: int [] []) =
        let maxX = octopuses.[0].Length - 1
        let maxY = octopuses.Length - 1

        for y in 0..maxY do
            for x in 0..maxX do
                if octopuses.[y].[x] >= 10 then
                    octopuses.[y].[x] <- 0

        octopuses

    let rec step n flashes octopuses =
        if n = 0 then
            flashes
        else
            let octopuses' = incAll octopuses
            let octopuses'' = flash octopuses' List.empty
            let flashesOfStep = countFlashers octopuses''
            let flashes' = flashes + flashesOfStep
            let octopuses''' = resetFlashers octopuses''
            step (n - 1) flashes' octopuses'''

    let day11 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
        |> step 100 0

    let runTillSync (octopuses: int [] []) =
        let octopusesCount = octopuses.Length * octopuses.[0].Length

        let rec step n octopuses =
            let octopuses' = incAll octopuses
            let octopuses'' = flash octopuses' List.empty
            let flashesOfStep = countFlashers octopuses''
            let octopuses''' = resetFlashers octopuses''

            if flashesOfStep = octopusesCount then
                n
            else
                step (n + 1) octopuses'''

        step 1 octopuses

    let day11Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
        |> runTillSync
