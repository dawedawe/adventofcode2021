namespace Adventofcode2021

module Day3 =

    [<Literal>]
    let InputFile = "Day3Input.txt"

    let count (numbers: string []) =
        let transposed =
            numbers
            |> Array.map (fun s -> s.ToCharArray())
            |> Array.transpose

        seq {
            for t in transposed do
                let counts = t |> Array.countBy id |> Map.ofArray
                let ones = counts.['1']
                let zeroes = counts.['0']

                if ones > zeroes then yield 1, 0
                else if zeroes > ones then yield 0, 1
                else failwith "no majority"
        }

    let fromBinary (bins: int seq) =
        let bins' = Seq.rev bins |> List.ofSeq

        [ for power in 0 .. List.length bins' - 1 ->
              let m = 2. ** (float power) |> int
              bins'.[power] * m ]
        |> List.sum

    let day3 () =
        let counts = InputFile |> System.IO.File.ReadAllLines |> count
        let gamma = counts |> Seq.map fst |> fromBinary
        let epsilon = counts |> Seq.map snd |> fromBinary
        gamma * epsilon
