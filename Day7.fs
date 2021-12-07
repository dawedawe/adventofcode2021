namespace Adventofcode2021

module Day7 =

    [<Literal>]
    let InputFile = "Day7Input.txt"

    let getMedian numbers =
        Array.get numbers ((Array.length numbers) / 2)

    let calcFuel numbers pos =
        numbers |> Array.sumBy (fun n -> abs (n - pos))

    let day7 () =
        let numbers =
            InputFile
            |> System.IO.File.ReadAllText
            |> fun s -> s.Split ','
            |> Array.map int
            |> Array.sort

        let median = getMedian numbers
        calcFuel numbers median
