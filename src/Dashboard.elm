module Dashboard exposing
    ( Model, init, subscriptions, Msg, update, view
    , Widget, widget
    )

{-| A dashboard library for moving and resizing widgets.


# Lifecycle

@docs Model, init, subscriptions, Msg, update, view


# Widgets

@docs Widget, widget

-}

import Browser.Dom as Dom
import Browser.Events as Window
import Dashboard.Internal.Layout as Layout
import Dashboard.Internal.Utils as Utils exposing (..)
import Dict
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events as Events
import Json.Decode as Json
import Task exposing (Task)


{-| a Task that will give you the initial dashboard model.
-}
init : Task Never Model
init =
    Dom.getViewport
        |> Task.map
            (\{ scene } ->
                { dragState = Nothing
                , windowWidth = round scene.width
                }
            )


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


type alias DashboardConfig a =
    { a
        | isDraggable : Bool
        , isResizable : Bool
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
    | StartDrag String Frame Operation Layout.Vector2
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
view : DashboardConfig {} -> Layout.Config { toMsg : Msg -> msg } -> Model -> List (Widget data msg) -> data -> Html msg
view config layout model widgets data =
    let
        notCurrent dragId (Widget wId _ _ _) =
            dragId /= wId

        areas =
            case model.dragState of
                Just state ->
                    let
                        current =
                            case List.filter (not << notCurrent state.widgetId) widgets |> List.head of
                                Just (Widget id frame toContent toAttributes) ->
                                    Widget id (addFrame frame state.frameDiff) toContent toAttributes

                                Nothing ->
                                    Widget "x" { x = 1, y = 1, height = 1, width = 1 } (\_ _ -> Html.text "") (\_ -> [])

                        correctedLayout =
                            getCorrectedLayout layout current widgets
                    in
                    toArea current :: correctedLayout

                Nothing ->
                    List.map toArea widgets

        frames =
            List.map (\a -> ( a.id, { x = a.x, y = a.y, height = a.height, width = a.width } )) areas |> Dict.fromList

        updateFrame ((Widget id _ toContent toAttributes) as w) =
            Dict.get id frames |> Maybe.map (\frame -> Widget id frame toContent toAttributes) |> Maybe.withDefault w

        updatedWidgets =
            List.map updateFrame widgets

        cellWidth =
            getCellWidth model layout
    in
    div
        [ --( "background-image"
          -- , if isDragging model then
          --  , gradient cellWidth
          --  """linear-gradient(0deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px),
          -- linear-gradient(-90deg, rgba(0,0,0,0.3), rgba(0,0,0,0.3) 0px, transparent 1px, transparent 7px, rgba(0,0,0,0.1) 7px, transparent 8px, transparent 15px, rgba(0,0,0,0.08) 15px, transparent 16px, transparent 23px, rgba(0,0,0,0.06) 23px, transparent 24px, transparent 31px, rgba(0,0,0,0.04) 31px, transparent 32px, transparent 39px, rgba(0,0,0,0.06) 39px, transparent 40px, transparent 47px, rgba(0,0,0,0.08) 47px, transparent 48px, transparent 55px, rgba(0,0,0,0.1) 55px, transparent 56px, transparent 63px, rgba(0,0,0,0.3) 63px, transparent 64px)"""
          -- else
          --     "none"
          --)
          style "transition" "all 0.5s"

        -- TODO: Adjust based on canvas size
        --, ( "background-size", px (cellWidth + config.gridGap) ++ " " ++ px (config.cellSize + config.gridGap) )
        --, ( "background-position", px config.marginLeft ++ " " ++ px config.marginTop )
        , style "width" "100%"
        , style "height" (px <| Layout.canvasHeight layout (Dict.values frames))
        , style "position" "relative"

        -- For stacking context
        , style "z-index" "1"
        , style "padding-top" (px layout.marginTop)
        , style "padding-right" (px layout.marginRight)
        , style "padding-bottom" (px layout.marginBottom)
        , style "padding-left" (px layout.marginLeft)
        ]
        [ div [ style "position" "relative", style "width" "100%", style "height" "100%" ]
            (List.concatMap (widgetView model config layout updatedWidgets model.dragState data) updatedWidgets)
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
update : DashboardConfig {} -> Layout.Config { toMsg : Msg -> msg } -> List (Widget data msg) -> Msg -> Model -> ( Model, Cmd Msg, Maybe (List ( String, { x : Int, y : Int, height : Int, width : Int } )) )
update config layout widgets msg model =
    case msg of
        Noop ->
            ( model, Cmd.none, Nothing )

        AdjustCanvas width ->
            ( { model | windowWidth = width }, Cmd.none, Nothing )

        StartDrag widgetId frame operation mousePoint ->
            let
                defaultDragState =
                    Just
                        { widgetId = widgetId
                        , frameDiff = { width = 0, height = 0, x = 0, y = 0 }
                        , startFrame = frame
                        , op = operation
                        , mouseStart = mousePoint
                        , mouseDiff = { x = 0, y = 0 }
                        }

                updatedModel =
                    { model
                        | dragState =
                            case ( operation, config.isDraggable, config.isResizable ) of
                                ( Move, True, _ ) ->
                                    defaultDragState

                                ( Resize _, _, True ) ->
                                    defaultDragState

                                _ ->
                                    Nothing
                    }
            in
            ( updatedModel
            , Cmd.none
            , Nothing
            )

        DragEnd ->
            case model.dragState of
                Just dragState ->
                    case List.head (List.filter (\(Widget id _ _ _) -> id == dragState.widgetId) widgets) of
                        Just selected ->
                            let
                                notCurrent dragId (Widget wId _ _ _) =
                                    dragId /= wId

                                areas =
                                    case model.dragState of
                                        Just state ->
                                            let
                                                current =
                                                    case List.filter (not << notCurrent state.widgetId) widgets |> List.head of
                                                        Just (Widget id frame toContent toAttributes) ->
                                                            Widget id (addFrame frame state.frameDiff) toContent toAttributes

                                                        Nothing ->
                                                            Widget "x" { x = 1, y = 1, height = 1, width = 1 } (\_ _ -> Html.text "") (\_ -> [])

                                                correctedLayout =
                                                    getCorrectedLayout layout current widgets
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
                            ( { model | dragState = Just { newState | frameDiff = Layout.move layout (getCellWidth model layout) dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

                        Resize direction ->
                            ( { model | dragState = Just { newState | frameDiff = Layout.resize direction layout (getCellWidth model layout) dragState.mouseStart mousePosition dragState.startFrame } }, Cmd.none, Nothing )

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
    = Widget String Frame (data -> Size -> Html msg) (data -> List (Html.Attribute msg))


{-| Creates a widget.
the `id` must be unique. `frame` in the size of the widget in grid dimensions.
`toContent` is a function that produces the widget's content. It is passed
the actual size of the widget so you can adjust the content.
-}
widget : String -> Frame -> (data -> Size -> Html msg) -> (data -> List (Html.Attribute msg)) -> Widget data msg
widget id frame toContent toAttributes =
    Widget id frame toContent toAttributes


toArea :
    Widget data msg
    -> { height : Int, id : String, width : Int, x : Int, y : Int }
toArea (Widget id frame _ _) =
    { id = id, x = frame.x, y = frame.y, height = frame.height, width = frame.width }


getCorrectedLayout :
    Layout.Config b
    -> Widget data msg
    -> List (Widget data1 msg1)
    -> List (Layout.Frame { id : String })
getCorrectedLayout config ((Widget id { x, y, width, height } toContent _) as current) widgets =
    let
        notCurrent (Widget wId _ _ _) =
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


toStyle isPhone phoneHeight { left, top, width, height } =
    if not isPhone then
        [ style "left" (px left)
        , style "top" (px top)
        , style "width" (px width)
        , style "height" (px height)
        ]

    else
        [ style "width" "100%"
        , style "margin-bottom" (px 20)
        , style "height" (px phoneHeight)
        ]


widgetView : Model -> DashboardConfig {} -> Layout.Config { toMsg : Msg -> msg } -> List (Widget data msg) -> Maybe DragState -> data -> Widget data msg -> List (Html msg)
widgetView model config ({ toMsg, cellSize, gridGap, columnCount } as layout) widgets dragState data ((Widget id frame toContent toAttributes) as current) =
    let
        { x, y, width, height } =
            frame

        correctedLayout =
            getCorrectedLayout layout current widgets

        isDraggingVar =
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
            case ( dragState, isDraggingVar ) of
                ( Just state, True ) ->
                    ( state.startFrame.height + state.frameDiff.height, state.startFrame.width + state.frameDiff.width )

                _ ->
                    ( height, width )

        cellWidth =
            getCellWidth model layout

        toGrid xVar =
            xVar * cellSize + ((xVar - 1) * gridGap)

        toGridWithGap xVar =
            xVar * cellSize + (xVar * gridGap)

        toGridWidth xVar =
            xVar * cellWidth + ((xVar - 1) * gridGap)

        toGridWidthWithGap xVar =
            xVar * cellWidth + (xVar * gridGap)

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
            case ( dragState, isDraggingVar ) of
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

        handles =
            [ div
                [ style "position" "absolute"
                , style "top" "-2px"
                , style "height" "4px"
                , style "left" "0"
                , style "right" "0"
                , if config.isResizable then
                    style "cursor" "ns-resize"

                  else
                    class ""
                , Events.custom "mousedown"
                    (Json.map
                        (\v -> { message = toMsg v, preventDefault = True, stopPropagation = True })
                     <|
                        Json.map (StartDrag id frame (Resize Layout.N)) mousePositionDecoder
                    )
                , Html.Attributes.id "top"
                ]
                []
            , div
                [ style "position" "absolute"
                , style "top" "-0"
                , style "width" "4px"
                , style "bottom" "0"
                , style "right" "-2px"
                , if config.isResizable then
                    style "cursor" "ew-resize"

                  else
                    class ""
                , Events.custom "mousedown"
                    (Json.map (\v -> { message = toMsg v, preventDefault = True, stopPropagation = True })
                        (Json.map (StartDrag id frame (Resize Layout.E)) mousePositionDecoder)
                    )
                ]
                []
            , div
                [ style "position" "absolute"
                , style "right" "0"
                , style "height" "4px"
                , style "bottom" "-2px"
                , style "left" "0"
                , if config.isResizable then
                    style "cursor" "ns-resize"

                  else
                    class ""
                , Events.custom "mousedown"
                    (Json.map (\v -> { message = toMsg v, preventDefault = True, stopPropagation = True })
                        (Json.map (StartDrag id frame (Resize Layout.S)) mousePositionDecoder)
                    )
                ]
                []
            , div
                [ style "position" "absolute"
                , style "top" "-0"
                , style "width" "4px"
                , style "bottom" "0"
                , style "left" "-2px"
                , if config.isResizable then
                    style "cursor" "ew-resize"

                  else
                    class ""
                , Events.custom "mousedown" (Json.map (\v -> { message = toMsg v, preventDefault = True, stopPropagation = True }) <| Json.map (StartDrag id frame (Resize Layout.W)) mousePositionDecoder)
                ]
                []
            , div
                [ style "position" "absolute"
                , style "width" "10px"
                , style "height" "10px"
                , style "bottom" "-5px"
                , style "right" "-5px"
                , if config.isResizable then
                    style "cursor" "nwse-resize"

                  else
                    class ""
                , Events.custom "mousedown"
                    (Json.map
                        (\v ->
                            { message = toMsg v
                            , preventDefault = True
                            , stopPropagation = True
                            }
                        )
                     <|
                        Json.map (StartDrag id frame (Resize Layout.SE)) mousePositionDecoder
                    )
                ]
                []
            , div
                [ style "position" "absolute"
                , style "width" "10px"
                , style "height" "10px"
                , style "bottom" "-5px"
                , style "left" "-5px"
                , if config.isResizable then
                    style "cursor" "nesw-resize"

                  else
                    class ""
                , Events.custom "mousedown"
                    (Json.map
                        (\v ->
                            { message = toMsg v
                            , preventDefault = True
                            , stopPropagation = True
                            }
                        )
                     <|
                        Json.map (StartDrag id frame (Resize Layout.SW)) mousePositionDecoder
                    )
                ]
                []
            ]
    in
    [ -- Move Shadow
      if isDraggingVar && not isPhone then
        div
            ([ style "border" "dashed 1px rgba(0,0,0,0.7)"
             , style "background-color" "rgba(0,0,0,0.1)"
             , style "position" "absolute"
             , style "box-sizing" "border-box"
             ]
                ++ toStyle isPhone phoneHeight actualDimensions
            )
            []

      else
        Html.text ""
    , div
        ([ style "background-color" "white"
         , style "position"
            (if isPhone then
                "relative"

             else
                "absolute"
            )
         , if isDraggingVar then
            style "z-index" "2"

           else
            class ""
         , if not isDraggingVar && dragState /= Nothing then
            style "transition" "all 0.2s ease"

           else
            class ""
         , Events.custom "mousedown" (Json.map (\v -> { message = toMsg v, preventDefault = True, stopPropagation = False }) <| Json.map (StartDrag id frame Move) mousePositionDecoder)
         ]
            ++ toStyle isPhone phoneHeight attrs
            ++ toAttributes data
        )
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
                            model.windowWidth - layout.marginLeft - layout.marginRight

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
        [ Window.onResize (\width _ -> AdjustCanvas width)
        , case model.dragState of
            Just _ ->
                Sub.batch
                    [ Window.onMouseMove (Json.map DragMove mousePositionDecoder)
                    , Window.onMouseUp (Json.succeed DragEnd)
                    ]

            Nothing ->
                Sub.none
        ]


mousePositionDecoder =
    Json.map2 Layout.Vector2 (Json.field "clientX" Json.int) (Json.field "clientY" Json.int)
