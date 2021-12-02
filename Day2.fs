namespace Adventofcode2021

module Day2 =

    [<Literal>]
    let InputFile = "Day2Input.txt"

    type Command =
        | Forward of int
        | Down of int
        | Up of int

    let parseLine (s: string) =
        let c = s.Split(' ')
        let x = int c[1]

        match c[0] with
        | "forward" -> Forward x
        | "down" -> Down x
        | "up" -> Up x
        | _ -> failwith $"unknown command {s}"

    let doCommand c (h, d) =
        match c with
        | Forward x -> (h + x, d)
        | Down x -> (h, d + x)
        | Up x -> (h, d - x)

    let rec drive f pos cmds =
        match cmds with
        | c :: rest ->
            let pos' = f c pos
            drive f pos' rest
        | [] -> pos

    let day2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> List.ofArray
        |> drive doCommand (0, 0)
        |> fun (h, d) -> h * d

    let doCommandPart2 c (h, d, a) =
        match c with
        | Forward x -> (h + x, d + a * x, a)
        | Down x -> (h, d, a + x)
        | Up x -> (h, d, a - x)

    let day2Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parseLine
        |> List.ofArray
        |> drive doCommandPart2 (0, 0, 0)
        |> fun (h, d, _) -> h * d
