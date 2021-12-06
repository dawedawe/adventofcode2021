namespace Adventofcode2021

module Day6 =

    [<Literal>]
    let InputFile = "Day6Input.txt"

    let decrease i = if i > 0 then i - 1 else 6

    let rec simulate days fish =
        if days > 0 then
            let newFish =
                fish
                |> List.sumBy (fun i -> if i = 0 then 1 else 0)
                |> fun length -> List.replicate length 8

            let fish' = fish |> List.map decrease
            let fish'' = List.append fish' newFish
            simulate (days - 1) fish''
        else
            fish

    let day6 () =
        InputFile
        |> System.IO.File.ReadAllText
        |> fun s -> s.Split(',')
        |> List.ofArray
        |> List.map int
        |> simulate 80
        |> List.length
