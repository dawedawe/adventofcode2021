namespace Adventofcode2021

module Day17 =

    [<Literal>]
    let InputFile = "Day17Input.txt"

    type TargetArea =
        { xStart: int
          xEnd: int
          yStart: int
          yEnd: int }

    let parse (s: string) =
        let regex = System.Text.RegularExpressions.Regex(@"([-]?\d)+")
        let matches = regex.Matches(s)

        { xStart = int matches.[0].Value
          xEnd = int matches.[1].Value
          yStart = int matches.[2].Value
          yEnd = int matches.[3].Value }

    let step (xPos, yPos) (xVel, yVel) =
        let xPos' = xPos + xVel
        let yPos' = yPos + yVel

        let xVel' =
            if xVel > 0 then xVel - 1
            else if xVel < 0 then xVel + 1
            else xVel

        let yVel' = yVel - 1
        (xPos', yPos'), (xVel', yVel')

    type PositionJudgement =
        | InTargetArea
        | InFlight
        | Overshoot

    let judgePosition targetArea (xPos, yPos) =
        if targetArea.xStart <= xPos
           && xPos <= targetArea.xEnd
           && targetArea.yStart <= yPos
           && yPos <= targetArea.yEnd then
            InTargetArea
        else if yPos < targetArea.yStart then
            Overshoot
        else
            InFlight

    let shoot pos velocity targetArea =
        let rec helper pos velocity targetArea yPath =
            let pos', velocity' = step pos velocity
            let yPath' = List.append yPath [ snd pos' ]

            match judgePosition targetArea pos' with
            | InTargetArea -> yPath' |> List.max |> Some
            | InFlight -> helper pos' velocity' targetArea yPath'
            | Overshoot -> None

        helper pos velocity targetArea List.empty

    let findVelocity targetArea =
        let startPos = (0, 0)

        seq {
            for x in 0..300 do
                for y in 0..300 do
                    let r = shoot startPos (x, y) targetArea

                    match r with
                    | Some y -> yield y
                    | None -> ()
        }
        |> Seq.max

    let day17 () =
        InputFile
        |> System.IO.File.ReadAllText
        |> parse
        |> findVelocity
