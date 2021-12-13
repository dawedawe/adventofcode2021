namespace Adventofcode2021

module Day13 =

    [<Literal>]
    let InputFile = "Day13Input.txt"

    let parseDotLine (s: string) =
        let split = s.Split(',')
        int split.[0], int split.[1]

    type FoldInstr =
        | Left of int
        | Up of int

    let parseInstrLine (s: string) =
        let idx = s.IndexOf('=')
        let d = s.[idx - 1]
        let n = s.Substring(idx + 1) |> int

        match d with
        | 'x' -> Left n
        | 'y' -> Up n
        | _ -> failwith "unknown input"

    let parse (lines: string []) =
        let dotLines =
            lines
            |> Array.takeWhile (fun s -> s.Length > 0)
            |> Array.map parseDotLine

        let instrLines =
            lines
            |> Array.skip (dotLines.Length + 1)
            |> Array.map parseInstrLine

        dotLines, instrLines

    let foldUp (dots: (int * int) []) foldLine =
        let upperDots = dots |> Array.filter (fun (_, y) -> y < foldLine)
        let lowerDots = dots |> Array.filter (fun (_, y) -> y > foldLine)

        let lowerDots' =
            lowerDots
            |> Array.map (fun (x, y) ->
                let dist = y - foldLine
                let y' = y - 2 * dist
                (x, y'))

        Array.append upperDots lowerDots'
        |> Array.distinct

    let foldLeft (dots: (int * int) []) foldLine =
        let leftDots = dots |> Array.filter (fun (x, _) -> x < foldLine)
        let rightDots = dots |> Array.filter (fun (x, _) -> x > foldLine)

        let rightDots' =
            rightDots
            |> Array.map (fun (x, y) ->
                let dist = x - foldLine
                let x' = x - 2 * dist
                (x', y))

        Array.append leftDots rightDots' |> Array.distinct

    let fold (dots: (int * int) []) instr =
        match instr with
        | Up y -> foldUp dots y
        | Left x -> foldLeft dots x

    let day13 () =
        let dots, instructions = InputFile |> System.IO.File.ReadAllLines |> parse
        let paper = fold dots instructions.[0]
        paper.Length

    let printPaper (dots: (int * int) []) =
        let maxX = dots |> Array.maxBy fst |> fst
        let maxY = dots |> Array.maxBy snd |> snd

        for y in 0..maxY do
            for x in 0..maxX do
                let c =
                    if (dots |> Array.contains (x, y)) then
                        '#'
                    else
                        ' '

                printf "%c" c

            printfn ""

    let rec foldPart2 (dots: (int * int) []) instr =
        if Array.isEmpty instr then
            dots
        else
            let dots' = fold dots instr.[0]
            let instr' = Array.tail instr
            foldPart2 dots' instr'

    let day13Part2 () =
        let dots, instructions = InputFile |> System.IO.File.ReadAllLines |> parse
        foldPart2 dots instructions |> printPaper
