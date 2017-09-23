module Dashboard.Internal.Layout exposing (..)

import Array exposing (Array)
import Dashboard.Internal.Data exposing (..)


widget :
    { id : String
    , x : Int
    , height : Int
    , width : Int
    }
    -> Widget
widget { id, x, width, height } =
    Widget
        { id = id
        , w = max width 1
        , h = max height 1
        , x = max 0 x
        }


unitConfig : Config data msg -> Config data msg
unitConfig (Config info) =
    Config { info | cellSize = 1, margin = 0 }


unitRects : Config data msg -> List Widget -> List Rect
unitRects config widgets =
    rects (unitConfig config) widgets


rects : Config data msg -> List Widget -> List Rect
rects ((Config { columnCount }) as config) widgets =
    let
        stack =
            Array.initialize columnCount (always 0)
    in
    rectsHelp config stack widgets []


rectsHelp : Config data msg -> Array.Array Int -> List Widget -> List Rect -> List Rect
rectsHelp ((Config { cellSize, margin }) as config) columnHeights widgets rects =
    case widgets of
        (Widget widget) :: rest ->
            let
                maxColumn =
                    widget.x + widget.w - 1

                widgetColumns =
                    Array.fromList
                        (List.range widget.x maxColumn)

                maxHeightInRange =
                    Array.foldl (\x m -> max x m)
                        0
                        (Array.slice widget.x (maxColumn + 1) columnHeights)

                newMaxHeight =
                    maxHeightInRange + widget.h

                updatedHeights =
                    Array.foldl (\i heights -> Array.set i newMaxHeight heights)
                        columnHeights
                        widgetColumns

                rect =
                    { w = widget.w * cellSize + (margin * (widget.w - 1))
                    , h = widget.h * cellSize + (margin * (widget.h - 1))
                    , x = widget.x * cellSize + (margin * (widget.x - 1))
                    , y = maxHeightInRange * cellSize + (margin * (maxHeightInRange - 1))
                    }
            in
            rectsHelp config updatedHeights rest (rect :: rects)

        [] ->
            List.reverse rects


{-| returns True if the two rects interfect.
-}
intersects : Rect -> Rect -> Bool
intersects r1 r2 =
    let
        between min max v =
            min <= v && v <= max

        xInRange =
            between r2.x (r2.x + r2.w) r1.x
                || between r2.x (r2.x + r2.w) (r1.x + r1.w)

        yInRange =
            between r2.y (r2.y + r2.h) (r1.y + 1)
                || between r2.y (r2.y + r2.h) (r1.y + r1.h + 1)
    in
    xInRange && yInRange


clampRectSize :
    Config data msg
    -> Rect
    -> Rect
clampRectSize (Config config) rect =
    { rect
        | w = clamp 1 config.columnCount rect.w
        , x = min rect.x (config.columnCount - rect.w)
        , y = max 0 rect.y
    }


pointToCoords : Config data msg -> { a | x : Int, y : Int } -> { x : Int, y : Int }
pointToCoords (Config config) { x, y } =
    let
        x1 =
            x + config.cellSize // 2

        y1 =
            y + config.cellSize // 2

        x_ =
            x1 // config.cellSize

        y_ =
            y1 // config.cellSize
    in
    { x = (x1 - config.margin * (x_ - 1)) // config.cellSize
    , y = (y1 - config.margin * (y_ - 1)) // config.cellSize
    }


unitRectForWidget : Config data msg -> String -> List Widget -> Maybe Rect
unitRectForWidget config widgetId widgets =
    List.map2 (,) (List.map (\(Widget { id }) -> id) widgets) (unitRects config widgets)
        |> List.filter (\( id, _ ) -> id == widgetId)
        |> List.head
        |> Maybe.map Tuple.second
