module Dashboard
    exposing
        ( Model
        , Msg
        , add
        , config
        , init
        , initWithWidgets
        , move
        , remove
        , resize
        , subscriptions
        , update
        , view
        )

{-| Create and layout dashboards in Elm.


# Configuration

@docs config


# Modification

@docs add, move, remove, resize, update, Msg


# View

@docs view, subscriptions


# State

@docs Model, init, initWithWidgets

-}

import Dashboard.Internal.Data as Types exposing (..)
import Dashboard.Internal.Layout as Layout
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Html.Keyed as Keyed
import Json.Decode as Json
import Mouse


{-| -}
type alias Model =
    { widgets : List Widget
    , dragState : DragState
    }


{-| -}
type Msg
    = Noop
    | DragStart String Corner ( Mouse.Position, Mouse.Position )
    | DragMove Mouse.Position
    | DragEnd Mouse.Position


{-| A helper function for creating valid configurations.
-}
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


{-| Creates the state that holds layout and dragging info.
-}
init : Model
init =
    { dragState = NotDragging
    , widgets = []
    }


{-| An initializer for copying dashboards
-}
initWithWidgets : List Widget -> Model
initWithWidgets widgets =
    { dragState = NotDragging
    , widgets = widgets
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


{-| The update function
-}
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
            case Layout.unitRectForWidget config id model.widgets of
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
                                    Layout.pointToCoords config info.start

                                b =
                                    Layout.pointToCoords config mousePosition

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
                                        (Layout.pointToCoords config mousePosition)
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
--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------


{-| Removes the widget with `id`. If the widget is not
found the dashboard is unchanged.
-}
remove :
    String
    -> List Widget
    -> List Widget
remove id dashboard =
    List.filter (\(Widget w) -> w.id /= id) dashboard


{-| -}
add : String -> Rect -> Config data msg -> List Widget -> List Widget
add id added config dashboard =
    let
        addedRestricted =
            Layout.clampRectSize config added

        layout =
            Layout.unitRects config dashboard

        firstAffectedIndex =
            List.indexedMap (,) layout
                |> List.filter
                    (\( i, existing ) -> Layout.intersects addedRestricted existing)
                |> List.head
                |> Maybe.map Tuple.first

        newWidget =
            Layout.widget
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


getById : String -> List Widget -> Maybe Widget
getById widgetId dashboard =
    List.filter (\(Widget w) -> w.id == widgetId) dashboard |> List.head


{-| Moves the widget with `id` to `newPoint`.
-}
move :
    String
    -> { a | x : Int, y : Int }
    -> Config data msg
    -> List Widget
    -> List Widget
move id newPoint config widgets =
    case getById id widgets of
        Just (Widget widget) ->
            remove id widgets
                |> add id
                    { x = newPoint.x
                    , y = newPoint.y
                    , w = widget.w
                    , h = widget.h
                    }
                    config

        Nothing ->
            widgets


{-| Changes the size of the widget with `widgetId` to match `newRect`.
The minimum size is 1x1.
-}
resize :
    String
    -> Rect
    -> Config data msg
    -> List Widget
    -> List Widget
resize widgetId newRect config widgets =
    case Layout.unitRectForWidget config widgetId widgets of
        Just prevRect ->
            remove widgetId widgets
                |> add widgetId
                    newRect
                    config

        Nothing ->
            widgets



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


{-| -}
view : Config data msg -> Model -> Html Msg
view config model =
    let
        -- It is important to render the widgets in the same order every time.
        -- Else the "move" animation jumps because the DOM elements move.
        -- The problem is that in the `rects` function the order dictates the
        -- y position of the widgets. We have to do the calculation before
        -- reordering by ID.
        ordered =
            List.map2 (,) model.widgets (Layout.rects config model.widgets) |> List.sortBy (\( Widget w, _ ) -> w.id)

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


{-| -}
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
