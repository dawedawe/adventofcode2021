namespace Adventofcode2021

module Day16 =

    [<Literal>]
    let InputFile = "Day16Input.txt"

    type Limit =
        | BitLimit of int * int
        | PackageLimit of int * int

    type LengthTypeId =
        | BitLength
        | PacketCount

    type TypeId =
        | Sum
        | Product
        | Min
        | Max
        | LiteralValue
        | Gt
        | Lt
        | Eq

    type Content =
        | LiteralValues of int64 list
        | OperatorSubPackages of Packet list

    and Packet =
        { Version: int
          TypeId: TypeId
          Contents: Content
          LengthTypeId: int option }

    let parseNibble =
        function
        | '0' -> [ 0; 0; 0; 0 ]
        | '1' -> [ 0; 0; 0; 1 ]
        | '2' -> [ 0; 0; 1; 0 ]
        | '3' -> [ 0; 0; 1; 1 ]
        | '4' -> [ 0; 1; 0; 0 ]
        | '5' -> [ 0; 1; 0; 1 ]
        | '6' -> [ 0; 1; 1; 0 ]
        | '7' -> [ 0; 1; 1; 1 ]
        | '8' -> [ 1; 0; 0; 0 ]
        | '9' -> [ 1; 0; 0; 1 ]
        | 'A' -> [ 1; 0; 1; 0 ]
        | 'B' -> [ 1; 0; 1; 1 ]
        | 'C' -> [ 1; 1; 0; 0 ]
        | 'D' -> [ 1; 1; 0; 1 ]
        | 'E' -> [ 1; 1; 1; 0 ]
        | 'F' -> [ 1; 1; 1; 1 ]
        | _ -> failwith "unknown nibble"

    let parse (s: string) =
        [ for i in 0 .. s.Length - 1 -> parseNibble s.[i] ]
        |> List.concat

    let parseFirst3Bits (bits: int list) =
        bits.[0] * 4 + bits.[1] * 2 + bits.[2] * 1

    let parseVersion = parseFirst3Bits

    let parseTypeId bits =
        match parseFirst3Bits bits with
        | 0 -> Sum
        | 1 -> Product
        | 2 -> Min
        | 3 -> Max
        | 4 -> LiteralValue
        | 5 -> Gt
        | 6 -> Lt
        | 7 -> Eq
        | _ -> failwith "unsupported typeid"

    let getGroups (bits: int list) =
        let rec helper (bits: int list) groups =
            let nextGroup = bits |> List.take 5
            let bits' = bits |> List.skip 5
            let groups' = List.append groups [ nextGroup |> List.skip 1 ]
            let isLastGroup = nextGroup.[0] = 0

            if isLastGroup then
                groups'
            else
                helper bits' groups'

        let groups = helper bits List.empty
        let bits' = bits |> List.skip (groups.Length * 5)
        groups, bits'

    let bitsValue (bits: int list) =
        seq {
            for i in 0 .. bits.Length - 1 do
                let factor = int64 (2. ** i)
                let bit = int64 bits.[bits.Length - 1 - i]
                yield bit * factor
        }
        |> Seq.sum

    let valueFromGroups groups = List.concat groups |> bitsValue


    let parseLiteralValue (bits: int list) =
        let groups, bits' = getGroups bits
        let value = valueFromGroups groups
        value, bits'

    let rec parseOperator (bits: int list) =
        let lengthTypeId =
            match bits.[0] with
            | 0 -> BitLength
            | 1 -> PacketCount
            | _ -> failwith "bad bit"

        let bits' = bits |> List.skip 1

        match lengthTypeId with
        | BitLength ->
            let lengthBits = bits' |> List.take 15
            let length = bitsValue lengthBits
            let bits'' = bits' |> List.skip 15
            let limit = Limit.BitLimit(0, int length) |> Some
            let subPackages, bits''' = parsePackage limit List.empty bits''
            subPackages, bits'''
        | PacketCount ->
            let lengthBits = bits' |> List.take 11
            let length = bitsValue lengthBits
            let bits'' = bits' |> List.skip 11
            let limit = Limit.PackageLimit(0, int length) |> Some
            let subPackages, bits''' = parsePackage limit List.empty bits''
            subPackages, bits'''

    and parsePackage (limit: Limit option) (packages: Packet list) (bits: int list) =
        if bits.Length <= 7
           && bits |> List.forall (fun b -> b = 0) then
            packages, List.empty
        else

            let version = bits |> List.take 3 |> parseVersion
            let bits' = bits |> List.skip 3
            let typeId = bits' |> List.take 3 |> parseTypeId

            let bits'' = bits' |> List.skip 3

            let (newPackages: Packet list), bits''' =
                match typeId with
                | TypeId.LiteralValue ->
                    let value, bits''' = parseLiteralValue bits''

                    let p: Packet =
                        { Version = version
                          TypeId = typeId
                          Contents = LiteralValues [ value ]
                          LengthTypeId = None }

                    [ p ], bits'''
                | _ ->
                    let subPackages, bits''' = parseOperator bits''

                    let p: Packet =
                        { Version = version
                          TypeId = typeId
                          Contents = OperatorSubPackages subPackages
                          LengthTypeId = None }

                    [ p ], bits'''

            let packages' = List.append packages newPackages

            let limit', limitReached =
                match limit with
                | None -> None, false
                | Some (Limit.BitLimit (x, n)) ->
                    let bitsParsed = bits.Length - bits'''.Length
                    let x' = x + bitsParsed
                    Some(Limit.BitLimit(x', n)), x' = n
                | Some (Limit.PackageLimit (x, n)) ->
                    let x' = x + newPackages.Length
                    Some(Limit.PackageLimit(x', n)), x' = n

            if List.isEmpty bits || limitReached then
                packages', bits'''
            else
                parsePackage limit' packages' bits'''

    let rec sumVersions sum (packages: Packet list) =
        match packages with
        | [] -> sum
        | p :: rest ->
            let sum' = sum + p.Version

            let subSum =
                match p.Contents with
                | LiteralValues _ -> 0
                | OperatorSubPackages subs -> sumVersions 0 subs

            let sum'' = sum' + subSum
            sumVersions sum'' rest

    let day16 () =
        InputFile
        |> System.IO.File.ReadAllText
        |> parse
        |> parsePackage None List.empty
        |> fst
        |> sumVersions 0

    let rec calc (p: Packet) =
        let values = p.Contents |> getValues

        match p.TypeId with
        | Sum -> values |> List.reduce (+)
        | Product -> values |> List.reduce (*)
        | Min -> values |> List.min
        | Max -> values |> List.max
        | Gt ->
            if values.[0] > values.[1] then
                1L
            else
                0L
        | Lt ->
            if values.[0] < values.[1] then
                1L
            else
                0L
        | Eq ->
            if values.[0] = values.[1] then
                1L
            else
                0L
        | LiteralValue -> values.[0]

    and getValues (contents: Content) =
        match contents with
        | Content.LiteralValues v -> v
        | Content.OperatorSubPackages p -> p |> List.map calc

    let day16Part2 () =
        InputFile
        |> System.IO.File.ReadAllText
        |> parse
        |> parsePackage None List.empty
        |> fst
        |> List.head
        |> calc
