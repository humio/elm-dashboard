module Dashboard
    exposing
        ( Config(..)
        , Dashboard
        , DragState(..)
        , Model
        , Msg
        , Widget(Widget)
        , add
        , config
        , init
        , move
        , pointToCoords
        , rects
        , remove
        , subscriptions
        , toString
        , update
        , view
        , widget
        )

import Array
import Dashboard.Internal.AsciiGrid as AsciiGrid
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Html.Keyed as Keyed
import Json.Decode as Json
import Mouse
import String


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


type alias DragInfo =
    { id : String
    , screenPosition : Mouse.Position
    , parentOffset : Mouse.Position
    , corner : Corner
    , start : Mouse.Position
    , originalUnitRect : Rect
    }


type Corner
    = Left
    | Right
    | None


type DragState
    = Dragging DragInfo
    | NotDragging


type alias Model =
    { widgets : List Widget
    , dragState : DragState
    }


type Msg
    = Noop
    | DragStart String Corner ( Mouse.Position, Mouse.Position )
    | DragMove Mouse.Position
    | DragEnd Mouse.Position


type alias ConfigProps data msg =
    { columnCount : Int
    , cellSize : Int
    , toWidgetContent : data -> Html msg
    , margin : Int
    }


type Config data msg
    = Config (ConfigProps data msg)


type alias WidgetProps =
    { id : String
    , x : Int
    , w : Int
    , h : Int
    }


type Widget
    = Widget WidgetProps


type alias Dashboard =
    List Widget


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


config : ConfigProps data msg -> Config data msg
config { columnCount, cellSize, toWidgetContent, margin } =
    Config
        { columnCount = max 1 columnCount
        , cellSize = max 1 cellSize
        , toWidgetContent = toWidgetContent
        , margin = max 0 margin
        }



--------------------------------------------------------------------------------
-- Init
--------------------------------------------------------------------------------


init : List Widget -> Model
init widgets =
    { dragState = NotDragging
    , widgets = widgets
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


update :
    Msg
    -> Config data msg
    -> Model
    -> ( Model, Cmd Msg )
update msg ((Config configInfo) as config) model =
    case msg of
        Noop ->
            model ! []

        DragStart id corner ( screenPosition, parentOffset ) ->
            case unitRectForWidget config id model.widgets of
                Nothing ->
                    model ! []

                Just originalUnitRect ->
                    { model
                        | dragState =
                            Dragging
                                { id = id
                                , screenPosition = { screenPosition | x = screenPosition.x - parentOffset.x, y = screenPosition.y - parentOffset.y }
                                , parentOffset = parentOffset
                                , corner = corner
                                , start = screenPosition
                                , originalUnitRect = originalUnitRect
                                }
                    }
                        ! []

        DragEnd _ ->
            { model | dragState = NotDragging } ! []

        DragMove uncorrectedMousePosition ->
            case model.dragState of
                Dragging info ->
                    let
                        mousePosition =
                            { uncorrectedMousePosition | x = uncorrectedMousePosition.x - info.parentOffset.x, y = uncorrectedMousePosition.y - info.parentOffset.y }
                    in
                    { model
                        | dragState = Dragging { info | screenPosition = mousePosition }
                        , widgets =
                            let
                                a =
                                    pointToCoords config info.start

                                b =
                                    pointToCoords config mousePosition

                                delta =
                                    { x = b.x - a.x, y = b.y - a.y }

                                unitRect =
                                    info.originalUnitRect

                                newRect =
                                    case info.corner of
                                        Left ->
                                            { unitRect | w = unitRect.w - delta.x, x = unitRect.x + delta.x, h = unitRect.h + delta.y }

                                        Right ->
                                            { unitRect | w = unitRect.w + delta.x, h = unitRect.h + delta.y }

                                        None ->
                                            unitRect
                            in
                            case info.corner of
                                None ->
                                    move info.id
                                        (pointToCoords config mousePosition)
                                        config
                                        model.widgets

                                Left ->
                                    resize info.id
                                        newRect
                                        config
                                        model.widgets

                                Right ->
                                    resize info.id
                                        newRect
                                        config
                                        model.widgets
                    }
                        ! []

                _ ->
                    model ! []



--------------------------------------------------------------------------------
-- Layout Logic
--------------------------------------------------------------------------------


type alias Rect =
    { w : Int
    , h : Int
    , x : Int
    , y : Int
    }


unitRects : Config data msg -> Dashboard -> List Rect
unitRects config widgets =
    rects (unitConfig config) widgets


rects : Config data msg -> Dashboard -> List Rect
rects ((Config { columnCount }) as config) widgets =
    let
        stack =
            Array.initialize columnCount (always 0)
    in
    rectsHelp config stack widgets []


rectsHelp : Config data msg -> Array.Array Int -> Dashboard -> List Rect -> List Rect
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



--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------


remove :
    String
    -> Dashboard
    -> Dashboard
remove id dashboard =
    List.filter (\(Widget w) -> w.id /= id) dashboard


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


unitConfig : Config data msg -> Config data msg
unitConfig (Config info) =
    Config { info | cellSize = 1, margin = 0 }


add : String -> Rect -> Config data msg -> Dashboard -> Dashboard
add id added config dashboard =
    let
        addedRestricted =
            clampRectSize config added

        layout =
            unitRects config dashboard

        firstAffectedIndex =
            List.indexedMap (,) layout
                |> List.filter
                    (\( i, existing ) -> intersects addedRestricted existing)
                |> List.head
                |> Maybe.map Tuple.first

        newWidget =
            widget
                { id = id
                , x = addedRestricted.x
                , width = addedRestricted.w
                , height = addedRestricted.h
                }
    in
    case firstAffectedIndex of
        Nothing ->
            List.append dashboard [ newWidget ]

        Just i ->
            List.concat
                [ List.take i dashboard
                , [ newWidget ]
                , List.drop i dashboard
                ]


getById : String -> Dashboard -> Maybe Widget
getById widgetId dashboard =
    List.filter (\(Widget w) -> w.id == widgetId) dashboard |> List.head


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


move :
    String
    -> { a | x : Int, y : Int }
    -> Config data msg
    -> Dashboard
    -> Dashboard
move widgetId newPoint config widgets =
    case getById widgetId widgets of
        Just (Widget widget) ->
            remove widgetId widgets
                |> add widgetId
                    { x = newPoint.x
                    , y = newPoint.y
                    , w = widget.w
                    , h = widget.h
                    }
                    config

        Nothing ->
            widgets


unitRectForWidget : Config data msg -> String -> List Widget -> Maybe Rect
unitRectForWidget config widgetId widgets =
    List.map2 (,) (List.map (\(Widget { id }) -> id) widgets) (unitRects config widgets)
        |> List.filter (\( id, _ ) -> id == widgetId)
        |> List.head
        |> Maybe.map Tuple.second


resize :
    String
    -> Rect
    -> Config data msg
    -> Dashboard
    -> Dashboard
resize widgetId newRect config widgets =
    case unitRectForWidget config widgetId widgets of
        Just prevRect ->
            remove widgetId widgets
                |> add widgetId
                    newRect
                    config

        Nothing ->
            widgets


toString : Config data msg -> Dashboard -> String
toString (Config config) dashboard =
    let
        layout =
            rects (Config { config | cellSize = 1 }) dashboard

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



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Config data msg -> Model -> Html Msg
view config model =
    let
        -- It is important to render the widgets in the same order every time.
        -- Else the "move" animation jumps because the DOM elements move.
        -- The problem is that in the `rects` function the order dictates the
        -- y position of the widgets. We have to do the calculation before
        -- reordering by ID.
        ordered =
            List.map2 (,) model.widgets (rects config model.widgets) |> List.sortBy (\( Widget w, _ ) -> w.id)

        orderedWidgets =
            List.map Tuple.first ordered

        orderedRects =
            List.map Tuple.second ordered
    in
    Keyed.node "div" [ style [ ( "position", "relative" ) ] ] (List.concat (List.map2 (viewWidget model) orderedWidgets orderedRects))


viewWidget : Model -> Widget -> Rect -> List ( String, Html Msg )
viewWidget model ((Widget info) as widget) rect =
    case model.dragState of
        NotDragging ->
            viewWidgetNotDragging model widget rect

        Dragging ({ parentOffset, id, corner } as state) ->
            if id == info.id && (corner /= None) then
                viewWidgetResizing state.screenPosition widget rect
            else if id == info.id then
                viewWidgetDragging state widget rect
            else
                viewWidgetNotDragging model widget rect


boxStyle : Rect -> Html.Attribute msg
boxStyle rect =
    style
        [ ( "position", "absolute" )
        , ( "transform", "translate(" ++ px rect.x ++ ", " ++ px rect.y ++ ")" )
        , ( "width", px rect.w )
        , ( "height", px rect.h )
        , ( "box-sizing", "border-box" )
        , ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "align-items", "stretch" )
        ]


viewWidgetNotDragging : Model -> Widget -> Rect -> List ( String, Html Msg )
viewWidgetNotDragging model ((Widget info) as widget) rect =
    [ ( info.id
      , Html.div
            [ boxStyle rect
            , style
                [ ( "background-color", "rgba(0,0,0,0.5)" )
                , ( "transition", "transform 0.5s" )
                , ( "border", "1px solid white" )
                ]
            , onWithOptions "mousedown" { stopPropagation = True, preventDefault = True } (Json.map (DragStart info.id None) decodeTargetPosition)
            ]
            [ content [ Html.text info.id ]
            , Html.button [ onWithOptions "mousedown" { stopPropagation = True, preventDefault = True } (Json.map (DragStart info.id Left) decodeTargetPositionParent), style [ ( "left", "0" ), ( "bottom", "0" ), ( "position", "absolute" ) ] ] [ Html.text "<-" ]
            , Html.button [ onWithOptions "mousedown" { stopPropagation = True, preventDefault = True } (Json.map (DragStart info.id Right) decodeTargetPositionParent), style [ ( "right", "0" ), ( "bottom", "0" ), ( "position", "absolute" ) ] ] [ Html.text "->" ]
            ]
      )
    ]


decodeTargetPosition : Json.Decoder ( Mouse.Position, Mouse.Position )
decodeTargetPosition =
    Json.map2 (,)
        Mouse.position
        (Json.map2 Mouse.Position
            (Json.at [ "offsetX" ] Json.int)
            (Json.at [ "offsetY" ] Json.int)
        )
        |> Json.andThen
            (\( screenOffset, targetOffset ) ->
                Json.map2 (,)
                    (Json.succeed screenOffset)
                    (Json.map2 Mouse.Position
                        (Json.at [ "currentTarget", "offsetParent", "offsetLeft" ] Json.int |> Json.map (\parentOffset -> targetOffset.x + parentOffset))
                        (Json.at [ "currentTarget", "offsetParent", "offsetTop" ] Json.int |> Json.map (\parentOffset -> targetOffset.y + parentOffset))
                    )
            )


decodeTargetPositionParent : Json.Decoder ( Mouse.Position, Mouse.Position )
decodeTargetPositionParent =
    decodeTargetPosition
        |> Json.andThen
            (\( screenOffset, targetOffset ) ->
                Json.map2 (,)
                    (Json.succeed screenOffset)
                    (Json.map2 Mouse.Position
                        (Json.at [ "currentTarget", "offsetLeft" ] Json.int |> Json.map (\parentOffset -> targetOffset.x + parentOffset))
                        (Json.at [ "currentTarget", "offsetTop" ] Json.int |> Json.map (\parentOffset -> targetOffset.y + parentOffset))
                    )
            )


viewWidgetDragging : DragInfo -> Widget -> Rect -> List ( String, Html Msg )
viewWidgetDragging { screenPosition } ((Widget info) as widget) rect =
    [ ( info.id
      , Html.div
            [ boxStyle { rect | x = screenPosition.x, y = screenPosition.y }
            , style
                [ ( "background-color", "#ffffff" )
                ]
            ]
            [ content [ Html.text info.id ] ]
      )
    , ( "________dragging"
      , Html.div
            [ boxStyle rect
            , style
                [ ( "border", "1px dashed gray" )
                ]
            ]
            []
      )
    ]


viewWidgetResizing : { a | x : Int, y : Int } -> Widget -> Rect -> List ( String, Html Msg )
viewWidgetResizing { x, y } ((Widget info) as widget) rect =
    [ ( info.id
      , Html.div
            [ boxStyle { rect | x = rect.x }
            , style
                [ ( "background-color", "#ffffff" )
                ]
            ]
            [ content [ Html.text info.id ] ]
      )
    ]


px : a -> String
px v =
    Basics.toString v ++ "px"


content : List (Html Msg) -> Html Msg
content inside =
    Html.div
        [ style
            [ ( "background-color", "#ffffff" )
            , ( "flex", "1 1 auto" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "stretch" )
            , ( "font-size", "30px" )
            , ( "color", "black " )
            ]
        ]
        [ Html.div [] inside ]



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Dragging _ ->
            Sub.batch
                [ Mouse.ups DragEnd
                , Mouse.moves DragMove
                ]

        NotDragging ->
            Sub.none
