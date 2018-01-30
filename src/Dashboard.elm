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

        cellWidth =
            getCellWidth model config
    in
    div
        [ style
            [ --( "background-image"
              -- , if isDragging model then
              --  , gradient cellWidth
              --  """linear-gradient(0deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px),
              -- linear-gradient(-90deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px)"""
              -- else
              --     "none"
              --)
              ( "transition", "all 0.5s" )

            -- TODO: Adjust based on canvas size
            --, ( "background-size", px (cellWidth + config.gridGap) ++ " " ++ px (config.cellSize + config.gridGap) )
            --, ( "background-position", px config.marginLeft ++ " " ++ px config.marginTop )
            , ( "width", "100%" )
            , ( "height", px <| Layout.canvasHeight config (Dict.values frames) )
            , ( "position", "relative" )

            -- For stacking context
            , ( "z-index", "1" )
            , ( "padding-top", px config.marginTop )
            , ( "padding-right", px config.marginRight )
            , ( "padding-bottom", px config.marginBottom )
            , ( "padding-left", px config.marginLeft )
            ]
        ]
        [ div [ style [ ( "position", "relative" ), ( "width", "100%" ), ( "height", "100%" ) ] ]
            (List.concatMap (widgetView model config updatedWidgets model.dragState data) updatedWidgets)
        ]


gradient : Int -> String
gradient width =
    let
        lineCount =
            8

        w =
            width // lineCount

        color i =
            if i == 0 || i == lineCount then
                "rgba(0,0,0,0.3)"
            else
                "rgba(0,0,0,0.1)"

        x =
            List.range 0 lineCount |> List.map (\i -> color i ++ " " ++ px (i * w) ++ ", transparent " ++ px (i * w + 1) ++ ", transparent " ++ px ((i + 1) * w))
    in
    "linear-gradient(90deg, rgba(0,0,0,0.3), " ++ String.join "," x ++ "), linear-gradient(0deg, rgba(0,0,0,0.3), " ++ String.join "," x ++ ")"


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
                            ( { model | dragState = Just { newState | frameDiff = Layout.move config (getCellWidth model config) dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

                        Resize direction ->
                            ( { model | dragState = Just { newState | frameDiff = Layout.resize direction config (getCellWidth model config) dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

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
            toArea current :: (widgets |> List.filter notCurrent |> List.map toArea |> List.sortBy .y)
    in
    Layout.correct config areas


getCellWidth :
    { a | windowWidth : Int }
    ->
        { b
            | columnCount : Int
            , gridGap : Int
            , marginLeft : Int
            , marginRight : Int
        }
    -> Int
getCellWidth model config =
    (getCanvasWidth model config - (config.gridGap * (config.columnCount - 1))) // config.columnCount


getCanvasWidth :
    { a | windowWidth : number }
    -> { b | marginLeft : number, marginRight : number }
    -> number
getCanvasWidth model config =
    model.windowWidth - config.marginLeft - config.marginRight


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

        cellWidth =
            getCellWidth model config

        toGrid x =
            x * cellSize + ((x - 1) * gridGap)

        toGridWithGap x =
            x * cellSize + (x * gridGap)

        toGridWidth x =
            x * cellWidth + ((x - 1) * gridGap)

        toGridWidthWithGap x =
            x * cellWidth + (x * gridGap)

        actualDimensions =
            { left = toGridWidthWithGap modifiedX
            , width = toGridWidth useWidth
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
                            { left = toGridWidthWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y + state.mouseDiff.y
                            , width = toGridWidth useWidth
                            , height = toGrid useHeight
                            }

                        Resize Layout.N ->
                            { left = toGridWidthWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y + state.mouseDiff.y
                            , width = toGridWidth state.startFrame.width
                            , height = toGrid state.startFrame.height - state.mouseDiff.y
                            }

                        Resize Layout.S ->
                            { left = toGridWidthWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGridWidth state.startFrame.width
                            , height = toGrid state.startFrame.height + state.mouseDiff.y
                            }

                        Resize Layout.E ->
                            { left = toGridWidthWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGridWidth state.startFrame.width + state.mouseDiff.x
                            , height = toGrid state.startFrame.height
                            }

                        Resize Layout.W ->
                            { left = toGridWidthWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGridWidth state.startFrame.width - state.mouseDiff.x
                            , height = toGrid state.startFrame.height
                            }

                        Resize Layout.SW ->
                            { left = toGridWidthWithGap state.startFrame.x + state.mouseDiff.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGridWidth state.startFrame.width - state.mouseDiff.x
                            , height = toGrid state.startFrame.height + state.mouseDiff.y
                            }

                        _ ->
                            { left = toGridWidthWithGap state.startFrame.x
                            , top = toGridWithGap state.startFrame.y
                            , width = toGridWidth state.startFrame.width + state.mouseDiff.x
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
                    , ( "margin-bottom", px 20 )
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
    [ -- Move Shadow
      if isDragging && not isPhone then
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
            , ( "z-index"
              , if isDragging then
                    "2"
                else
                    "1"
              )
            ]
        , style
            (if not isDragging && dragState /= Nothing then
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
                            model.windowWidth - config.marginLeft - config.marginRight
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
