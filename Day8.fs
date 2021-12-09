namespace Adventofcode2021

module Day8 =

    [<Literal>]
    let InputFile = "Day8Input.txt"

    let parse (s: string) = s.Split(' ')[11..14]

    let count (outputs: string []) =
        outputs
        |> Array.sumBy (fun o ->
            match o.Length with
            | 2
            | 3
            | 4
            | 7 -> 1
            | _ -> 0)

    let day8 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.map count
        |> Array.sum

    let allUsedSegments (inputs: string []) =
        inputs
        |> Array.collect (fun s -> s.ToCharArray())
        |> Array.distinct
        |> Set.ofArray

    let deduce (inputs: string []) =
        let allUsed = allUsedSegments inputs

        let segs2 =
            inputs
            |> Array.filter (fun a -> a.Length = 2)
            |> allUsedSegments

        let segs3 =
            inputs
            |> Array.filter (fun a -> a.Length = 3)
            |> allUsedSegments

        let segs4 =
            inputs
            |> Array.filter (fun a -> a.Length = 4)
            |> allUsedSegments

        let aCandidates = Set.difference segs3 segs2

        let bCandidates =
            Set.difference segs4 segs2
            |> fun s -> Set.difference s segs3
            |> fun s -> Set.difference s aCandidates

        let cCandidates = Set.difference segs3 aCandidates

        let eCandidates =
            Set.difference allUsed segs2
            |> fun s -> Set.difference s segs3
            |> fun s -> Set.difference s segs4
            |> fun s -> Set.difference s aCandidates

        let fCandidates = Set.difference segs3 aCandidates

        let d069Candidates = inputs |> Array.filter (fun a -> a.Length = 6)

        let d06Candidates =
            d069Candidates
            |> Array.filter (fun a -> not (segs4.IsSubsetOf(a.ToCharArray() |> Set.ofArray)))

        let d0Candidates =
            d06Candidates
            |> Array.filter (fun a -> (segs2.IsSubsetOf(a.ToCharArray() |> Set.ofArray)))
            |> Array.item 0
            |> fun x -> x.ToCharArray() |> Set.ofArray

        let bCandidates = Set.intersect d0Candidates bCandidates

        let dCandidates =
            Set.difference segs4 segs2
            |> fun s -> Set.difference s bCandidates

        let d9Candidates =
            d069Candidates
            |> Array.filter (fun c -> not (Array.contains c d06Candidates))
            |> Array.head
            |> fun s -> s.ToCharArray()
            |> Set.ofArray

        let gCandidates =
            Set.difference d9Candidates aCandidates
            |> fun s -> Set.difference s bCandidates
            |> fun s -> Set.difference s dCandidates
            |> fun s -> Set.difference s segs2

        let eCandidates = Set.difference eCandidates gCandidates

        let a = aCandidates |> Set.toArray |> Array.head
        let b = bCandidates |> Set.toArray |> Array.head
        let d = dCandidates |> Set.toArray |> Array.head
        let e = eCandidates |> Set.toArray |> Array.head
        let g = gCandidates |> Set.toArray |> Array.head

        let d2Candidates =
            inputs
            |> Array.filter (fun a -> a.Length = 5)
            |> Array.filter (fun s ->
                s.Contains(a)
                && s.Contains(d)
                && s.Contains(e)
                && s.Contains(g))
            |> Array.map (fun s -> s.ToCharArray())
            |> Array.head
            |> Set.ofArray

        let fCandidates = Set.difference fCandidates d2Candidates
        let cCandidates = Set.difference cCandidates fCandidates

        let c = cCandidates |> Set.toArray |> Array.head
        let f = fCandidates |> Set.toArray |> Array.head

        [ a; b; c; d; e; f; g ]

    let displayValue (mapping: char list) (output: string) =
        let onOffs =
            seq {
                for m in mapping do
                    yield output.Contains(m)
            }
            |> Seq.toList

        match onOffs with
        | [ true; true; true; false; true; true; true ] -> 0
        | [ false; false; true; false; false; true; false ] -> 1
        | [ true; false; true; true; true; false; true ] -> 2
        | [ true; false; true; true; false; true; true ] -> 3
        | [ false; true; true; true; false; true; false ] -> 4
        | [ true; true; false; true; false; true; true ] -> 5
        | [ true; true; false; true; true; true; true ] -> 6
        | [ true; false; true; false; false; true; false ] -> 7
        | [ true; true; true; true; true; true; true ] -> 8
        | [ true; true; true; true; false; true; true ] -> 9
        | _ -> failwith "unknown display"

    let displayValues (inputs, outputs) =
        let mapping = deduce inputs

        outputs
        |> Array.map (displayValue mapping)
        |> fun v -> v.[0] * 1000 + v.[1] * 100 + v.[2] * 10 + v.[3]

    let parsePart2 (s: string) =
        let inputs = s.Split(' ')[0..9]
        let outputs = s.Split(' ')[11..14]

        let inputs' =
            inputs
            |> Array.map (fun s -> s.ToCharArray() |> Array.sort |> System.String)

        inputs', outputs

    let day8Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parsePart2
        |> Array.map displayValues
        |> Array.sum
