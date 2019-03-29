module Dashboard.Internal.Layout exposing
    ( Config
    , Direction(..)
    , Frame
    , Vector2
    , canvasHeight
    , correct
    , isOverlapping
    , minusPoint
    , move
    , resize
    )

import Browser.Events as Events
import Json.Decode as D


type alias Frame a =
    { a
        | width : Int
        , height : Int
        , x : Int
        , y : Int
    }


type alias Config a =
    { a
        | cellSize : Int
        , columnCount : Int
        , gridGap : Int
        , marginTop : Int
        , marginRight : Int
        , marginBottom : Int
        , marginLeft : Int
    }


type alias Vector2 =
    { x : Int, y : Int }


correct : Config b -> List (Frame a) -> List (Frame a)
correct { columnCount } widgets =
    let
        restrict a =
            { a
                | x = clamp 0 (columnCount - a.width) a.x
                , y = max 0 a.y
                , width = clamp 1 columnCount a.width
                , height = max 1 a.height
            }

        moveOverlapping a rest fixed completed =
            case rest of
                b :: newRest ->
                    let
                        fixedB =
                            if isOverlapping a b then
                                { b | y = a.y + a.height }

                            else
                                b
                    in
                    moveOverlapping a newRest (fixedB :: fixed) completed

                [] ->
                    case List.reverse fixed of
                        [] ->
                            List.reverse (a :: completed)

                        next :: newRest ->
                            moveOverlapping next newRest [] (a :: completed)

        updatedWidgets =
            case List.map restrict widgets of
                hd :: rest ->
                    moveOverlapping hd rest [] []

                [] ->
                    []
    in
    updatedWidgets


minusPoint : Vector2 -> Vector2 -> Vector2
minusPoint a b =
    { x = a.x - b.x
    , y = a.y - b.y
    }


move : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
move config cellWidth mouseOrigin mousePosition frame =
    let
        yStepSize =
            config.gridGap + config.cellSize // 2

        xStepSize =
            config.gridGap + cellWidth // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        xDirection =
            if mouseDiff.x > 0 then
                1

            else
                -1

        xMax =
            config.columnCount - frame.x - frame.width

        xMin =
            -frame.x

        xDiff =
            if mouseDiff.x < xStepSize && mouseDiff.x > -xStepSize then
                0

            else
                ((mouseDiff.x - xStepSize * xDirection) // (cellWidth + config.gridGap) + 1 * xDirection)
                    |> clamp xMin xMax

        yDirection =
            if mouseDiff.y > 0 then
                1

            else
                -1

        yMin =
            -frame.y

        yDiff =
            if mouseDiff.y < yStepSize && mouseDiff.y > -yStepSize then
                0

            else
                ((mouseDiff.y - yStepSize * yDirection) // (config.cellSize + config.gridGap) + 1 * yDirection)
                    |> max yMin
    in
    { x = xDiff, y = yDiff, width = 0, height = 0 }


type Direction
    = N
    | E
    | W
    | SW
    | S
    | NE
    | NW
    | SE


resize : Direction -> Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resize direction =
    case direction of
        N ->
            resizeN

        SE ->
            resizeSE

        E ->
            resizeE

        W ->
            resizeW

        S ->
            resizeS

        SW ->
            resizeSW

        _ ->
            resizeN


resizeSW : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeSW config cellWidth mouseOrigin mousePosition frame =
    let
        yStepSize =
            config.gridGap + config.cellSize // 2

        xStepSize =
            config.gridGap + cellWidth // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        xDirection =
            if mouseDiff.x > 0 then
                1

            else
                -1

        xMax =
            config.columnCount - frame.x - frame.width

        xMin =
            -frame.width + 1

        xDiff =
            if mouseDiff.x < xStepSize && mouseDiff.x > -xStepSize then
                0

            else
                ((mouseDiff.x - xStepSize * xDirection) // (cellWidth + config.gridGap) + xDirection)
                    |> clamp xMin xMax

        yDirection =
            if mouseDiff.y > 0 then
                1

            else
                -1

        yMin =
            -frame.height + 1

        yDiff =
            if mouseDiff.y < yStepSize && mouseDiff.y > -yStepSize then
                0

            else
                ((mouseDiff.y - yStepSize * yDirection) // (config.cellSize + config.gridGap) + yDirection)
                    |> max yMin
    in
    { x = xDiff
    , y = 0
    , width = -xDiff
    , height = yDiff
    }


resizeSE : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeSE config cellWidth mouseOrigin mousePosition frame =
    let
        yStepSize =
            config.gridGap + config.cellSize // 2

        xStepSize =
            config.gridGap + cellWidth // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        xDirection =
            if mouseDiff.x > 0 then
                1

            else
                -1

        xMax =
            config.columnCount - frame.x - frame.width

        xMin =
            -frame.width + 1

        xDiff =
            if mouseDiff.x < xStepSize && mouseDiff.x > -xStepSize then
                0

            else
                ((mouseDiff.x - xStepSize * xDirection) // (cellWidth + config.gridGap) + xDirection)
                    |> clamp xMin xMax

        yDirection =
            if mouseDiff.y > 0 then
                1

            else
                -1

        yMin =
            -frame.height + 1

        yDiff =
            if mouseDiff.y < yStepSize && mouseDiff.y > -yStepSize then
                0

            else
                ((mouseDiff.y - yStepSize * yDirection) // (config.cellSize + config.gridGap) + yDirection)
                    |> max yMin
    in
    { x = 0
    , y = 0
    , width = xDiff
    , height = yDiff
    }


resizeN : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeN config cellWidth mouseOrigin mousePosition frame =
    let
        stepSize =
            config.gridGap + config.cellSize // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        yDirection =
            if mouseDiff.y > 0 then
                1

            else
                -1

        yMin =
            -frame.x

        yMax =
            frame.height - 1

        yDiff =
            if mouseDiff.y < stepSize && mouseDiff.y > -stepSize then
                0

            else
                ((mouseDiff.y - stepSize * yDirection) // (config.cellSize + config.gridGap) + yDirection)
                    |> clamp yMin yMax
    in
    { x = 0
    , y = yDiff
    , width = 0
    , height = -yDiff
    }


resizeS : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeS config cellWidth mouseOrigin mousePosition frame =
    let
        stepSize =
            config.gridGap + config.cellSize // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        yDirection =
            if mouseDiff.y > 0 then
                1

            else
                -1

        yMin =
            -frame.height + 1

        yDiff =
            if mouseDiff.y < stepSize && mouseDiff.y > -stepSize then
                0

            else
                ((mouseDiff.y - stepSize * yDirection) // (config.cellSize + config.gridGap) + yDirection)
                    |> max yMin
    in
    { x = 0
    , y = 0
    , width = 0
    , height = yDiff
    }


resizeW : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeW config cellWidth mouseOrigin mousePosition frame =
    let
        stepSize =
            config.gridGap + cellWidth // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        xDirection =
            if mouseDiff.x > 0 then
                1

            else
                -1

        xMin =
            -frame.x

        xMax =
            frame.width - 1

        xDiff =
            if mouseDiff.x < stepSize && mouseDiff.x > -stepSize then
                0

            else
                ((mouseDiff.x - stepSize * xDirection) // (cellWidth + config.gridGap) + xDirection)
                    |> clamp xMin xMax
    in
    { x = xDiff
    , y = 0
    , width = -xDiff
    , height = 0
    }


resizeE : Config b -> Int -> Vector2 -> Vector2 -> Frame a -> Frame {}
resizeE config cellWidth mouseOrigin mousePosition frame =
    let
        stepSize =
            config.gridGap + cellWidth // 2

        mouseDiff =
            minusPoint mousePosition mouseOrigin

        xDirection =
            if mouseDiff.x > 0 then
                1

            else
                -1

        xMax =
            config.columnCount - frame.x - frame.width

        xMin =
            -frame.width + 1

        xDiff =
            if mouseDiff.x < stepSize && mouseDiff.x > -stepSize then
                0

            else
                ((mouseDiff.x - stepSize * xDirection) // (cellWidth + config.gridGap) + xDirection)
                    |> clamp xMin xMax
    in
    { x = 0
    , y = 0
    , width = xDiff
    , height = 0
    }


canvasHeight : Config b -> List (Frame a) -> Int
canvasHeight config frames =
    List.maximum (List.map (\f -> f.y + f.height) frames)
        |> Maybe.withDefault 0
        |> (\max -> max * config.cellSize + (max - 1) * config.gridGap)
        |> ((+) 50) -- HACK: There is something off with the height. Add some padding (Feels bad man)


isOverlapping : Frame a -> Frame b -> Bool
isOverlapping a b =
    -- If one rectangle is on left side of other
    a.x < b.x + b.width && a.x + a.width > b.x && a.y < b.y + b.height && a.y + a.height > b.y
