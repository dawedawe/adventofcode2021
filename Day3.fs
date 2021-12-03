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
                let ones = counts.TryFind '1' |> Option.defaultValue 0
                let zeroes = counts.TryFind '0' |> Option.defaultValue 0

                if ones > zeroes then yield 1, 0
                else if zeroes > ones then yield 0, 1
                else yield -1, -1
        }
        |> Seq.toArray

    let fromBinary (bins: int []) =
        let bins' = Array.rev bins

        [ for power in 0 .. Array.length bins' - 1 ->
              let m = 2. ** (float power) |> int
              bins'.[power] * m ]
        |> List.sum

    let day3 () =
        let counts = InputFile |> System.IO.File.ReadAllLines |> count
        let gamma = counts |> Array.map fst |> fromBinary
        let epsilon = counts |> Array.map snd |> fromBinary
        gamma * epsilon

    let rec filter pos (bitCriteria: int * int -> int) keeper (numbers: string []) =
        let counts = numbers |> count

        let bitToFilterBy =
            if (counts.[pos] = (-1, -1)) then
                keeper
            else
                bitCriteria counts.[pos]

        let numbers' =
            numbers
            |> Array.filter (fun s -> int (string s.[pos]) = bitToFilterBy)

        if numbers'.Length = 1 then
            numbers'.[0].ToCharArray()
            |> Array.map (string >> int)
        else
            filter (pos + 1) bitCriteria keeper numbers'

    let day3Part2 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let ox = filter 0 fst 1 lines |> fromBinary
        let co2 = filter 0 snd 0 lines |> fromBinary
        ox * co2
