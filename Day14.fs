namespace Adventofcode2021

module Day14 =

    [<Literal>]
    let InputFile = "Day14Input.txt"

    let parseRuleLine (line: string) =
        line.Substring(0, 2), line.Substring(6, 1)

    let parseInput (lines: string []) =
        let template = lines.[0]

        let ruleLines =
            lines
            |> Array.skip 2
            |> Array.map parseRuleLine
            |> Map.ofArray

        template, ruleLines

    let step (s: string) (rules: Map<string, string>) =
        let pairs =
            s.ToCharArray()
            |> Array.windowed 2
            |> Array.map (fun a -> $"{a.[0]}{a.[1]}")

        seq {
            for i in 0 .. pairs.Length - 1 do
                let pair = pairs.[i]
                let ins = rules.[pair]

                if i = 0 then
                    $"{pair.[0]}{ins}{pair.[1]}"
                else
                    $"{ins}{pair.[1]}"
        }
        |> String.concat ""

    let rec steps n (template, rules) =
        if n = 0 then
            template
        else
            let template' = step template rules
            steps (n - 1) (template', rules)

    let score (s: string) =
        s.ToCharArray()
        |> Array.countBy id
        |> Array.sortBy snd
        |> fun a -> snd (Array.last a) - snd (Array.head a)

    let day14 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> parseInput
        |> steps 10
        |> score
