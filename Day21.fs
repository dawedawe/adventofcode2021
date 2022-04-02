namespace Adventofcode2021

module Day21 =

    [<Literal>]
    let InputFile = "Day21Input.txt"

    type Player = { Id: int; Pos: int; Score: int }

    let parseLine (s: string) =
        let words = s.Split(' ')

        { Id = int words[1]
          Pos = int words[4]
          Score = 0 }

    type Dice = { Rolls: int; Last: int }

    let rollDice dice =
        let e1 = dice.Last + 1
        let e2 = dice.Last + 2
        let e3 = dice.Last + 3
        let dice' = { Rolls = dice.Rolls + 3; Last = e3 }
        (dice', e1 + e2 + e3)

    let calcPos p toMove =
        let p' = ((p + toMove) % 10)
        if p' = 0 then 10 else p'

    let makeRound player dice =
        let (dice', r) = rollDice dice
        let pos = calcPos player.Pos r

        let player' =
            { player with
                Score = player.Score + pos
                Pos = pos }

        (player', dice')

    let rec game p1 p2 dice =
        let (p1', dice') = makeRound p1 dice

        if p1'.Score >= 1000 then
            p2.Score * dice'.Rolls
        else
            let (p2', dice'') = makeRound p2 dice'

            if p2'.Score >= 1000 then
                p1'.Score * dice''.Rolls
            else
                game p1' p2' dice''

    let day21 () =
        let dice = { Rolls = 0; Last = 0 }

        let players =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine

        game players[0] players[1] dice
