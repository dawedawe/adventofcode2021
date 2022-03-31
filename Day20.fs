namespace Adventofcode2021

module Day20 =

    [<Literal>]
    let InputFile = "Day20Input.txt"

    let getPixel fallback (inputImage: string array) (x, y) =
        if x < 0
           || y < 0
           || x >= inputImage[0].Length
           || y >= inputImage.Length then
            fallback
        else
            inputImage[y][x]

    let toBinary (pixels: char list) =
        pixels
        |> List.map (function
            | '.' -> 0
            | '#' -> 1
            | _ -> failwith "bad input")

    let toInt (bits: int list) =
        bits
        |> List.rev
        |> List.indexed
        |> List.sumBy (fun (i, b) -> b * int (2. ** i))

    let getIndex fallback (inputImage: string array) (x, y) =
        [ (x - 1, y - 1)
          (x, y - 1)
          (x + 1, y - 1)
          (x - 1, y)
          (x, y)
          (x + 1, y)
          (x - 1, y + 1)
          (x, y + 1)
          (x + 1, y + 1) ]
        |> List.map (getPixel fallback inputImage)
        |> toBinary
        |> toInt

    let enhance fallback (algo: string) (inputImage: string array) =
        [| for y in -1 .. inputImage.Length do
               [| for x in -1 .. inputImage[0].Length do
                      let idx = getIndex fallback inputImage (x, y)
                      yield algo[idx] |]
               |> System.String |]

    let calcFallback (algo: string) previous =
        previous
        |> List.replicate 9
        |> toBinary
        |> toInt
        |> fun i -> algo[i]

    let rec enhanceN n fallback (algo: string) (inputImage: string array) =
        if n = 0 then
            inputImage
        else
            let image = enhance fallback algo inputImage
            let fallback' = calcFallback algo fallback
            enhanceN (n - 1) fallback' algo image

    let day20 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let algo = lines[0]
        let picture = lines[2..]

        enhanceN 2 '.' algo picture
        |> Array.sumBy (fun line ->
            line.ToCharArray()
            |> Array.sumBy (fun c -> if c = '#' then 1 else 0))

    let day20Part2 () =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let algo = lines[0]
        let picture = lines[2..]

        enhanceN 50 '.' algo picture
        |> Array.sumBy (fun line ->
            line.ToCharArray()
            |> Array.sumBy (fun c -> if c = '#' then 1 else 0))
