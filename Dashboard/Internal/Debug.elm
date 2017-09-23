module Dashboard.Internal.Debug exposing (..)

import Dashboard.Internal.AsciiGrid as AsciiGrid
import Dashboard.Internal.Data exposing (..)
import Dashboard.Internal.Layout as Layout


toString : Config data msg -> List Widget -> String
toString (Config config) dashboard =
    let
        layout =
            Layout.rects (Config { config | cellSize = 1 }) dashboard

        maxY : Int
        maxY =
            layout |> List.map (\rect -> rect.y + rect.h) |> List.foldl max 0

        grid =
            { w = config.columnCount + 1, h = maxY }

        firstLetter id =
            id
                |> String.toList
                |> List.head
                |> Maybe.withDefault '?'

        layoutWithCharNames =
            List.map2
                (\(Widget w) r ->
                    { id = firstLetter w.id
                    , x = r.x
                    , y = r.y
                    , h = r.h
                    , w = r.w
                    }
                )
                dashboard
                layout
    in
    AsciiGrid.toString grid layoutWithCharNames
