namespace Adventofcode2021

module Day7 =

    [<Literal>]
    let InputFile = "Day7Input.txt"

    let getMedian numbers =
        Array.get numbers ((Array.length numbers) / 2)

    let calcFuel f numbers pos =
        numbers |> Array.sumBy (fun n -> f pos n)

    let numbers =
        InputFile
        |> System.IO.File.ReadAllText
        |> fun s -> s.Split ','
        |> Array.map int
        |> Array.sort

    let f pos n = abs (n - pos)

    let day7 () = getMedian numbers |> calcFuel f numbers

    let f2 pos n =
        let d = abs (n - pos)
        Seq.init d (id >> (+) 1) |> Seq.sum

    let day7Part2 () =
        let first = numbers.[0]
        let last = Array.last numbers
        let fuel = [ first..last ] |> Seq.minBy (calcFuel f2 numbers)
        calcFuel f2 numbers fuel
