namespace Adventofcode2021

module Day6 =

    [<Literal>]
    let InputFile = "Day6Input.txt"

    let decrease i = if i > 0 then i - 1 else 6

    let rec simulate days (fishAgesAndCounts: (int * int64) list) =
        if days = 0 then
            fishAgesAndCounts |> List.sumBy snd
        else
            let newFishCount =
                fishAgesAndCounts
                |> Map.ofList
                |> Map.tryFind 0
                |> Option.defaultValue 0

            let fishAgesAndCounts' =
                fishAgesAndCounts
                |> List.map (fun (a, c) -> (decrease a, c))
                |> List.groupBy fst
                |> List.map (fun (age, ageCountList) -> (age, ageCountList |> List.sumBy snd))
                |> Map.ofList
                |> Map.add 8 newFishCount

            simulate (days - 1) (fishAgesAndCounts' |> Map.toList)

    let initialFish path =
        path
        |> System.IO.File.ReadAllText
        |> fun s -> s.Split(',')
        |> List.ofArray
        |> List.map int
        |> List.countBy id
        |> List.map (fun (a, c) -> (a, int64 c))

    let day6 () = InputFile |> initialFish |> simulate 80

    let day6Part2 () =
        InputFile |> initialFish |> simulate 256
