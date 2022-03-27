namespace Adventofcode2021

module Day19 =

    [<Literal>]
    let InputFile = "Day19Input.txt"

    type Pos = { X: int; Y: int; Z: int }

    type Scanner = { Id: int; Pos: Pos; Beacons: Pos [] }

    let parsePosLine (s: string) =
        s.Split(',')
        |> Array.map int
        |> fun a -> { X = a[0]; Y = a[1]; Z = a[2] }

    let rec parse (lines: string []) =
        if lines.Length = 0 then
            List.empty
        else
            let scannerLines =
                lines
                |> Array.takeWhile (System.String.IsNullOrWhiteSpace >> not)

            let id =
                scannerLines[0]
                    .Replace("--- scanner ", "")
                    .Replace(" ---", "")
                |> int

            let positions = scannerLines[1..] |> Array.map parsePosLine

            let scanner =
                { Id = id
                  Pos = { X = 0; Y = 0; Z = 0 }
                  Beacons = positions }

            let rest =
                lines
                |> Array.skipWhile (fun s -> s <> $"--- scanner {id + 1} ---")

            scanner :: (parse rest)

    let turnXYPlus90 p = { X = -(p.Y); Y = p.X; Z = p.Z }
    let turnXYPlus180 = turnXYPlus90 >> turnXYPlus90
    let turnXYPlus270 = turnXYPlus90 >> turnXYPlus90 >> turnXYPlus90

    let turnXYMinus90 p = { X = p.Y; Y = -(p.X); Z = p.Z }
    let turnXYMinus180 = turnXYMinus90 >> turnXYMinus90
    let turnXYMinus270 = turnXYMinus90 >> turnXYMinus90 >> turnXYMinus90

    let turnXZPlus90 p = { X = -(p.Z); Y = p.Y; Z = p.X }
    let turnXZPlus180 = turnXZPlus90 >> turnXZPlus90
    let turnXZPlus270 = turnXZPlus90 >> turnXZPlus90 >> turnXZPlus90

    let turnXZMinus90 p = { X = p.Z; Y = p.Y; Z = -(p.X) }
    let turnXZMinus180 = turnXZMinus90 >> turnXZMinus90
    let turnXZMinus270 = turnXZMinus90 >> turnXZMinus90 >> turnXZMinus90

    let turnYZPlus90 p = { X = p.X; Y = p.Z; Z = -(p.Y) }
    let turnYZPlus180 = turnYZPlus90 >> turnYZPlus90
    let turnYZPlus270 = turnYZPlus90 >> turnYZPlus90 >> turnYZPlus90

    let turnYZMinus90 p = { X = p.X; Y = -(p.Z); Z = p.Y }
    let turnYZMinus180 = turnYZMinus90 >> turnYZMinus90
    let turnYZMinus270 = turnYZMinus90 >> turnYZMinus90 >> turnYZMinus90

    let turnXZMinus90YMinus180 = turnXYMinus90 >> turnYZMinus90
    let turnXZPlus90YPlus270 = turnXYPlus90 >> turnYZPlus90 >> turnYZPlus90
    let turnXZMinus90YMinus270 = turnXYMinus90 >> turnYZMinus90 >> turnYZMinus90
    let turnXYMinus90YZPlus90 = turnXYMinus90 >> turnYZPlus90
    let turnXYPlus90YZPlus90YZMinus90 = turnXYPlus90 >> turnYZPlus90 >> turnYZMinus90
    let turnXYMinus90YZMinus90YZPlus90 = turnXYMinus90 >> turnYZMinus90 >> turnYZPlus90
    let turnXYPlus90YZMinus90YZPlus90 = turnXYPlus90 >> turnYZMinus90 >> turnYZPlus90
    let turnXYMinus90YZPlus90YZMinus90 = turnXYMinus90 >> turnYZPlus90 >> turnYZMinus90

    let transformations =
        [ id
          turnXYPlus90
          turnXYPlus180
          turnXYPlus270

          turnXYMinus90
          turnXYMinus180
          turnXYMinus270

          turnXZPlus90
          turnXZPlus180
          turnXZPlus270

          turnXZMinus90
          turnXZMinus180
          turnXZMinus270

          turnYZPlus90
          turnYZPlus180
          turnYZPlus270

          turnYZMinus90
          turnYZMinus180
          turnYZMinus270

          turnXYPlus90 >> turnXZPlus90
          turnXYPlus180 >> turnXZPlus90
          turnXYPlus270 >> turnXZPlus90

          turnXYPlus90 >> turnXZMinus90
          turnXYPlus180 >> turnXZMinus90
          turnXYPlus270 >> turnXZMinus90

          turnXYPlus90 >> turnYZPlus90
          turnXYPlus180 >> turnYZPlus90
          turnXYPlus270 >> turnYZPlus90

          turnXYPlus90 >> turnYZMinus90
          turnXYPlus180 >> turnYZMinus90
          turnXYPlus270 >> turnYZMinus90

          turnYZPlus90 >> turnXYMinus90
          turnYZPlus90 >> turnXYMinus180
          turnYZPlus90 >> turnXYMinus270

          turnYZPlus90 >> turnXYPlus90
          turnYZPlus90 >> turnXYPlus180
          turnYZPlus90 >> turnXYPlus270

          turnYZMinus90 >> turnXYMinus90
          turnYZMinus90 >> turnXYMinus180
          turnYZMinus90 >> turnXYMinus270

          turnYZMinus90 >> turnXYPlus90
          turnYZMinus90 >> turnXYPlus180
          turnYZMinus90 >> turnXYPlus270

          turnXZMinus90YMinus180
          turnXZPlus90YPlus270
          turnXZMinus90YMinus270
          turnXYMinus90YZPlus90
          turnXYPlus90YZPlus90YZMinus90
          turnXYMinus90YZMinus90YZPlus90
          turnXYPlus90YZMinus90YZPlus90
          turnXYMinus90YZPlus90YZMinus90 ]

    let calcDiff p1 p2 =
        (p1.X - p2.X), (p1.Y - p2.Y), (p1.Z - p2.Z)

    let calcDiffsForPos positions p =
        positions
        |> Array.map (calcDiff p)
        |> Array.filter (fun d -> d <> (0, 0, 0))

    let isOverlappingPos p1Diffs p2Diffs =
        p1Diffs
        |> Array.filter (fun d -> p2Diffs |> Array.contains d)
        |> Array.length
        |> (<=) 11

    let getOverlappingPosCore (scanner1: Scanner) (scanner2: Scanner) =
        seq {
            for p1 in scanner1.Beacons do
                let p1Diffs = calcDiffsForPos scanner1.Beacons p1

                for p2 in scanner2.Beacons do
                    let p2Diffs = calcDiffsForPos scanner2.Beacons p2

                    if isOverlappingPos p1Diffs p2Diffs then
                        yield (p1, p2)
        }
        |> Seq.toList

    let getOverlappingPos (scanner1: Scanner) (scanner2: Scanner) =
        let rec helper ts =
            match ts with
            | [] -> None
            | t :: rest ->
                let beacons = scanner2.Beacons |> Array.map t
                let scanner2' = { scanner2 with Beacons = beacons }
                let overlaps = getOverlappingPosCore scanner1 scanner2'

                if List.isEmpty overlaps then
                    helper rest
                else
                    Some(scanner2', overlaps)

        helper transformations

    let getRelativePos { X = x0; Y = y0; Z = z0 } { X = x1; Y = y1; Z = z1 } =
        { X = x0 - x1
          Y = y0 - y1
          Z = z0 - z1 }

    let normalizeScanner scanner relPos =
        let beacons' =
            scanner.Beacons
            |> Array.map (fun b ->
                { X = b.X + relPos.X
                  Y = b.Y + relPos.Y
                  Z = b.Z + relPos.Z })

        { scanner with
            Pos = relPos
            Beacons = beacons' }

    let tryNormalizeScanner s0 s1 =
        let s1TransformedAndOverlaps = getOverlappingPos s0 s1

        match s1TransformedAndOverlaps with
        | Some (s1', overlaps) ->
            let relPos = getRelativePos (fst overlaps[0]) (snd overlaps[0])
            let normalizedScanner = normalizeScanner s1' relPos
            Some normalizedScanner
        | None -> None

    let findPositions (scanners: Scanner list) =
        let mutable normalizedScanners = [ scanners[0] ]
        let mutable toDoScanners = scanners.Tail
        let mutable beaconsSet = Set.ofArray scanners[0].Beacons

        while not toDoScanners.IsEmpty do
            let pairs = List.allPairs normalizedScanners toDoScanners

            for (s0, s1) in pairs do
                if (normalizedScanners
                    |> List.exists (fun x -> x.Id = s1.Id)
                    |> not) then
                    let s1' = tryNormalizeScanner s0 s1

                    match s1' with
                    | Some normS ->
                        normalizedScanners <- List.append normalizedScanners [ normS ]
                        beaconsSet <- Set.union beaconsSet (Set.ofArray normS.Beacons)

                        toDoScanners <-
                            toDoScanners
                            |> List.filter (fun s -> s.Id <> normS.Id)
                    | None -> ()

        (normalizedScanners, beaconsSet)

    let day19 () =
        let scanners = InputFile |> System.IO.File.ReadAllLines |> parse
        let (_, beaconsSet) = findPositions scanners
        beaconsSet.Count

    let manhattanDistance (pos0, pos1) =
        abs  ((max pos0.X pos1.X) - (min pos0.X pos1.X))
        + abs ((max pos0.Y pos1.Y) - (min pos0.Y pos1.Y))
        + abs ((max pos0.Z pos1.Z) - (min pos0.Z pos1.Z))

    let day19Part2 () =
        let scanners = InputFile |> System.IO.File.ReadAllLines |> parse
        let (normalizedScanners, _) = findPositions scanners
        let scannerPositions =
            normalizedScanners
            |> List.map (fun s -> s.Pos)
        List.allPairs scannerPositions scannerPositions
        |> List.map manhattanDistance
        |> List.max
