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
        let sb = System.Text.StringBuilder()

        for i in 0 .. s.Length - 2 do
            let pair = sprintf "%c%c" s.[i] s.[i + 1]
            let ins = rules.[pair]

            if i = 0 then
                sb.Append($"{pair.[0]}{ins}{pair.[1]}") |> ignore
            else
                sb.Append($"{ins}{pair.[1]}") |> ignore

        sb.ToString()

    let steps n (template, rules) =
        let rec helper n (template: string, rules) =
            if n = 0 then
                template
            else
                let template' = step template rules
                helper (n - 1) (template', rules)

        helper n (template, rules)

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

    let expand (pairs: Map<string, int64>) (rules: Map<string, string>) =
        let mutable newPairs = Map.empty<string, int64>

        for pairKeyValue in pairs do
            let ins = rules.[pairKeyValue.Key]
            let expansion = sprintf "%c%s%c" pairKeyValue.Key.[0] ins pairKeyValue.Key.[1]
            let newpair1 = expansion.Substring(0, 2)
            let newpair2 = expansion.Substring(1, 2)

            if newPairs.ContainsKey newpair1 then
                newPairs <- Map.add newpair1 (newPairs.[newpair1] + pairKeyValue.Value) newPairs
            else
                newPairs <- Map.add newpair1 pairKeyValue.Value newPairs

            if newPairs.ContainsKey newpair2 then
                newPairs <- Map.add newpair2 (newPairs.[newpair2] + pairKeyValue.Value) newPairs
            else
                newPairs <- Map.add newpair2 pairKeyValue.Value newPairs

        newPairs

    let rec run n (pairs: Map<string, int64>) (rules: Map<string, string>) =
        if n = 0 then
            pairs
        else
            let pairs' = expand pairs rules
            run (n - 1) pairs' rules

    let scorePart2 (pairs: Map<string, int64>) =
        let mutable counts = Map.empty<char, int64>

        for pair in pairs do
            let a = pair.Key.[0]

            if counts.ContainsKey a then
                counts <- Map.add a (counts.[a] + pair.Value) counts
            else
                counts <- Map.add a pair.Value counts

        counts
        |> Map.toSeq
        |> Seq.sortBy snd
        |> fun s -> snd (Seq.last s) - snd (Seq.head s)

    let day14Part2 () =
        let template, rules =
            InputFile
            |> System.IO.File.ReadAllLines
            |> parseInput

        let pairs =
            template.ToCharArray()
            |> Array.windowed 2
            |> Array.map (fun a -> sprintf "%c%c" a.[0] a.[1])
            |> Array.countBy id
            |> Array.map (fun (a, b) -> a, int64 b)
            |> Map.ofArray

        run 40 pairs rules
        |> scorePart2
        |> fun r -> r - 1L
