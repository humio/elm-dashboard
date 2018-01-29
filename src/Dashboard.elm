module Dashboard exposing (Model, Msg, Widget, init, subscriptions, update, view, widget)

{-| A dashboard library for moving and resizing widgets.


# Lifecycle

@docs Model, init, subscriptions, Msg, update, view


# Widgets

@docs Widget, widget

-}

import Dashboard.Internal.Layout as Layout
import Dashboard.Internal.Utils as Utils exposing (..)
import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Mouse
import Task exposing (Task)
import Window


{-| a Task that will give you the initial dashboard model.
-}
init : Task Never Model
init =
    Window.width |> Task.map (Model Nothing)


type alias Point =
    { x : Int, y : Int }


type alias DragState =
    { widgetId : String
    , startFrame : Frame
    , frameDiff : Frame
    , mouseStart : Point
    , mouseDiff : Point
    , op : Operation
    }


{-| -}
type alias Model =
    { dragState : Maybe DragState
    , windowWidth : Int
    }


type Operation
    = Resize Layout.Direction
    | Move


{-| -}
type Msg
    = Noop
    | StartDrag String Frame Operation Mouse.Position
    | DragEnd
    | DragMove { x : Int, y : Int }
    | AdjustCanvas Int


addFrame : Frame -> Frame -> Frame
addFrame a b =
    { x = a.x + b.x
    , y = a.y + b.y
    , height = a.height + b.height
    , width = a.width + b.width
    }


fullCanvasWidth : Layout.Config a -> Int
fullCanvasWidth config =
    config.cellSize * config.columnCount + config.gridGap * (config.columnCount - 1)


{-| -}
view : Layout.Config { toMsg : Msg -> msg } -> Model -> List (Widget data msg) -> data -> Html msg
view config model widgets data =
    let
        notCurrent dragId (Widget wId _ _) =
            dragId /= wId

        areas =
            case model.dragState of
                Just state ->
                    let
                        current =
                            case List.filter (not << notCurrent state.widgetId) widgets |> List.head of
                                Just (Widget id frame toContent) ->
                                    Widget id (addFrame frame state.frameDiff) toContent

                                Nothing ->
                                    Widget "x" { x = 1, y = 1, height = 1, width = 1 } (\_ _ -> Html.text "")

                        correctedLayout =
                            getCorrectedLayout config current widgets
                    in
                    toArea current :: correctedLayout

                Nothing ->
                    List.map toArea widgets

        frames =
            List.map (\a -> ( a.id, { x = a.x, y = a.y, height = a.height, width = a.width } )) areas |> Dict.fromList

        updateFrame ((Widget id _ toContent) as w) =
            Dict.get id frames |> Maybe.map (\frame -> Widget id frame toContent) |> Maybe.withDefault w

        updatedWidgets =
            List.map updateFrame widgets
    in
    div
        [ style
            [ ( "background-image"
              , if isDragging model then
                    """linear-gradient(0deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px),
                   linear-gradient(-90deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px)"""
                else
                    "none"
              )
            , ( "transition", "all 0.5s" )
            ]
        , style
            -- TODO: Adjust based on canvas size
            [ ( "background-size", "74px 74px" )
            , ( "background-position", "-10px -10px" )
            , ( "width", px <| config.cellSize * config.columnCount + config.gridGap * (config.columnCount - 1) )
            , ( "height", px <| Layout.canvasHeight config (Dict.values frames) )
            , ( "position", "relative" )
            ]
        ]
        (List.concatMap (widgetView model config updatedWidgets model.dragState data) updatedWidgets)


getCellPoint : Point -> Point -> { a | width : Int } -> Int -> Point
getCellPoint elementOffset clientOffset { width } columnCount =
    let
        leftCornerX =
            clientOffset.x - elementOffset.x

        cellX =
            clamp 0 (columnCount - width) <| round (toFloat leftCornerX / toFloat (cellSize + gapSize))

        leftCornerY =
            clientOffset.y - elementOffset.y

        cellY =
            max 0 <| round (toFloat leftCornerY / toFloat (cellSize + gapSize))
    in
    { x = cellX, y = cellY }


{-| -}
update : Layout.Config { toMsg : Msg -> msg } -> List (Widget data msg) -> Msg -> Model -> ( Model, Cmd Msg, Maybe (List ( String, { x : Int, y : Int, height : Int, width : Int } )) )
update config widgets msg model =
    case msg of
        Noop ->
            ( model, Cmd.none, Nothing )

        AdjustCanvas width ->
            ( { model | windowWidth = width }, Cmd.none, Nothing )

        StartDrag widgetId frame operation mousePoint ->
            ( { model
                | dragState =
                    Just
                        { widgetId = widgetId
                        , frameDiff = { width = 0, height = 0, x = 0, y = 0 }
                        , startFrame = frame
                        , op = operation
                        , mouseStart = mousePoint
                        , mouseDiff = { x = 0, y = 0 }
                        }
              }
            , Cmd.none
            , Nothing
            )

        DragEnd ->
            case model.dragState of
                Just dragState ->
                    case List.head (List.filter (\(Widget id _ _) -> id == dragState.widgetId) widgets) of
                        Just selected ->
                            let
                                notCurrent dragId (Widget wId _ _) =
                                    dragId /= wId

                                areas =
                                    case model.dragState of
                                        Just state ->
                                            let
                                                current =
                                                    case List.filter (not << notCurrent state.widgetId) widgets |> List.head of
                                                        Just (Widget id frame toContent) ->
                                                            Widget id (addFrame frame state.frameDiff) toContent

                                                        Nothing ->
                                                            Widget "x" { x = 1, y = 1, height = 1, width = 1 } (\_ _ -> Html.text "")

                                                correctedLayout =
                                                    getCorrectedLayout config current widgets
                                            in
                                            toArea current :: correctedLayout

                                        Nothing ->
                                            List.map toArea widgets

                                frames =
                                    List.map (\a -> ( a.id, { x = a.x, y = a.y, height = a.height, width = a.width } )) areas
                            in
                            ( { model | dragState = Nothing }, Cmd.none, Just frames )

                        Nothing ->
                            ( model, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        DragMove mousePosition ->
            case model.dragState of
                Just dragState ->
                    let
                        mouseDiff =
                            Layout.minusPoint mousePosition dragState.mouseStart

                        newState =
                            { dragState | mouseDiff = mouseDiff }
                    in
                    case dragState.op of
                        Move ->
                            ( { model | dragState = Just { newState | frameDiff = Layout.move config dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

                        Resize direction ->
                            ( { model | dragState = Just { newState | frameDiff = Layout.resize direction config dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

                Nothing ->
                    ( model, Cmd.none, Nothing )


type alias Size =
    { width : Int, height : Int }


type alias Frame =
    { x : Int
    , y : Int
    , height : Int
    , width : Int
    }


{-| -}
type Widget data msg
    = Widget String Frame (data -> Size -> Html msg)


{-| Creates a widget.
the `id` must be unique. `frame` in the size of the widget in grid dimensions.
`toContent` is a function that produces the widget's content. It is passed
the actual size of the widget so you can adjust the content.
-}
widget : String -> Frame -> (data -> Size -> Html msg) -> Widget data msg
widget id frame toContent =
    Widget id frame toContent


cellSize : number
cellSize =
    64


gapSize : number
gapSize =
    10


toArea :
    Widget data msg
    -> { height : Int, id : String, width : Int, x : Int, y : Int }
toArea (Widget id frame _) =
    { id = id, x = frame.x, y = frame.y, height = frame.height, width = frame.width }


getCorrectedLayout :
    Layout.Config b
    -> Widget data msg
    -> List (Widget data1 msg1)
    -> List (Layout.Frame { id : String })
getCorrectedLayout config ((Widget id { x, y, width, height } toContent) as current) widgets =
    let
        notCurrent (Widget wId _ _) =
            id /= wId

        areas =
            toArea current :: List.map toArea (List.filter notCurrent widgets)
    in
    Layout.correct config areas


widgetView : Model -> Layout.Config { toMsg : Msg -> msg } -> List (Widget data msg) -> Maybe DragState -> data -> Widget data msg -> List (Html msg)
widgetView model ({ toMsg, cellSize, gridGap, columnCount } as config) widgets dragState data ((Widget id frame toContent) as current) =
    let
        { x, y, width, height } =
            frame

        correctedLayout =
            getCorrectedLayout config current widgets

        isDragging =
            case dragState of
                Just state ->
                    state.widgetId == id

                Nothing ->
                    False

        ( modifiedX, modifiedY ) =
            case dragState of
                Just state ->
                    if state.widgetId == id then
                        ( state.startFrame.x + state.frameDiff.x, state.startFrame.y + state.frameDiff.y )
                    else
                        ( x, y )

                Nothing ->
                    ( x, y )

        ( useHeight, useWidth ) =
            case ( dragState, isDragging ) of
                ( Just state, True ) ->
                    ( state.startFrame.height + state.frameDiff.height, state.startFrame.width + state.frameDiff.width )

                _ ->
                    ( height, width )

        toGrid x =
            x * cellSize + ((x - 1) * gridGap)

        toGridWithGap x =
            x * cellSize + (x * gridGap)

        actualDimensions =
            { left = toGridWithGap modifiedX
            , width = toGrid useWidth
            , top = toGridWithGap modifiedY
            , height = toGrid useHeight
            }

        isPhone =
            model.windowWidth < 900

        phoneHeight =
            260

        attrs =
            case ( dragState, isDragging ) of
                ( Just state, True ) ->
                    case state.op of
                        Move ->
                            { left = toGridWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y + state.mouseDiff.y
                            , width = toGrid useWidth
                            , height = toGrid useHeight
                            }

                        Resize Layout.N ->
                            { left = toGridWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y + state.mouseDiff.y
                            , width = toGrid state.startFrame.width
                            , height = toGrid state.startFrame.height - state.mouseDiff.y
                            }

                        Resize Layout.S ->
                            { left = toGridWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGrid state.startFrame.width
                            , height = toGrid state.startFrame.height + state.mouseDiff.y
                            }

                        Resize Layout.E ->
                            { left = toGridWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGrid state.startFrame.width + state.mouseDiff.x
                            , height = toGrid state.startFrame.height
                            }

                        Resize Layout.W ->
                            { left = toGridWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGrid state.startFrame.width - state.mouseDiff.x
                            , height = toGrid state.startFrame.height
                            }

                        Resize Layout.SW ->
                            { left = toGridWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGrid state.startFrame.width - state.mouseDiff.x
                            , height = toGrid state.startFrame.height + state.mouseDiff.y
                            }

                        _ ->
                            { left = toGridWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGrid state.startFrame.width + state.mouseDiff.x
                            , height = toGrid state.startFrame.height + state.mouseDiff.y
                            }

                _ ->
                    actualDimensions

        toStyle { left, top, width, height } =
            style <|
                if not isPhone then
                    [ ( "left", px left )
                    , ( "top", px top )
                    , ( "width", px width )
                    , ( "height", px height )
                    ]
                else
                    [ ( "width", "100%" )
                    , ( "margin-bottom", px 30 )
                    , ( "height", px phoneHeight )
                    ]

        handles =
            [ div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "-2px" )
                    , ( "height", "4px" )
                    , ( "left", "0" )
                    , ( "right", "0" )
                    , ( "cursor", "ns-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.N)) Mouse.position)
                , Html.Attributes.id "top"
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "-0" )
                    , ( "width", "4px" )
                    , ( "bottom", "0" )
                    , ( "right", "-2px" )
                    , ( "cursor", "ew-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.E)) Mouse.position)
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "right", "0" )
                    , ( "height", "4px" )
                    , ( "bottom", "-2px" )
                    , ( "left", "0" )
                    , ( "cursor", "ns-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.S)) Mouse.position)
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "-0" )
                    , ( "width", "4px" )
                    , ( "bottom", "0" )
                    , ( "left", "-2px" )
                    , ( "cursor", "ew-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.W)) Mouse.position)
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "width", "10px" )
                    , ( "height", "10px" )
                    , ( "bottom", "-5px" )
                    , ( "right", "-5px" )
                    , ( "cursor", "nwse-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.SE)) Mouse.position)
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "width", "10px" )
                    , ( "height", "10px" )
                    , ( "bottom", "-5px" )
                    , ( "left", "-5px" )
                    , ( "cursor", "nesw-resize" )
                    ]
                , onWithOptions "mousedown" { preventDefault = True, stopPropagation = True } (Json.map toMsg <| Json.map (StartDrag id frame (Resize Layout.SW)) Mouse.position)
                ]
                []
            ]
    in
    [ if isDragging && not isPhone then
        div
            [ style
                [ ( "border", "dashed 1px rgba(0,0,0,0.7)" )
                , ( "background-color", "rgba(0,0,0,0.1)" )
                , ( "position", "absolute" )
                , ( "box-sizing", "border-box" )
                ]
            , toStyle actualDimensions
            ]
            []
      else
        Html.text ""
    , div
        [ style
            [ ( "background-color", "white" )
            , ( "position"
              , if isPhone then
                    "relative"
                else
                    "absolute"
              )
            ]
        , style
            (if not isDragging then
                [ ( "transition", "all 0.2s ease" ) ]
             else
                []
            )
        , toStyle attrs
        , onWithOptions "mousedown" { preventDefault = True, stopPropagation = False } (Json.map toMsg <| Json.map (StartDrag id frame Move) Mouse.position)
        ]
      <|
        handles
            ++ [ toContent data
                    { height =
                        if isPhone then
                            phoneHeight
                        else
                            attrs.height
                    , width =
                        if isPhone then
                            model.windowWidth
                        else
                            attrs.width
                    }
               ]
    ]


isDragging : Model -> Bool
isDragging dashboard =
    dashboard.dragState /= Nothing


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (.width >> AdjustCanvas)
        , case model.dragState of
            Just _ ->
                Sub.batch
                    [ Mouse.moves DragMove
                    , Mouse.ups (\_ -> DragEnd)
                    ]

            Nothing ->
                Sub.none
        ]
