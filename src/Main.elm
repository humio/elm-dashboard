module Main exposing (main)

import Browser exposing (Document)
import Dashboard
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Task


type alias WidgetProps =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { dashboard : Maybe Dashboard.Model
    , widgets : Dict String WidgetProps
    }


type Msg
    = NoOp
    | DashboardMsg Dashboard.Msg
    | Load Dashboard.Model


init : () -> ( Model, Cmd Msg )
init _ =
    let
        loadDashboard =
            Dashboard.init { isDraggable = True, isResizable = True }
    in
    ( { dashboard = Nothing
      , widgets =
            Dict.fromList
                [ ( "a", { x = 0, y = 4, width = 3, height = 4 } )
                , ( "b", { x = 6, y = 2, width = 4, height = 2 } )
                , ( "c", { x = 3, y = 4, width = 3, height = 3 } )
                ]
      }
    , Task.perform Load loadDashboard
    )


toWidgets : Dict String WidgetProps -> List (Dashboard.Widget a msg)
toWidgets widgets =
    Dict.map
        (\id frame ->
            Dashboard.widget id
                frame
                (\data size ->
                    Html.div
                        [ style "border" "solid 1px black"
                        , style "height" (String.fromInt size.height ++ "px")
                        , style "width" (String.fromInt size.width ++ "px")
                        ]
                        [ Html.text <| id ++ " (" ++ Debug.toString size ++ ")" ]
                )
                (\data -> [])
        )
        widgets
        |> Dict.values


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        Load dashboard ->
            ( { model | dashboard = Just dashboard }
            , Cmd.none
            )

        DashboardMsg submsg ->
            case model.dashboard of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just dashboard ->
                    let
                        ( updatedDashboard, cmd, updatedWidgetList ) =
                            Dashboard.update dashboardConfig
                                (toWidgets model.widgets)
                                submsg
                                dashboard

                        updatedWidgets =
                            case updatedWidgetList of
                                Just newPositions ->
                                    List.foldl
                                        (\( widgetId, position ) result ->
                                            Dict.update widgetId
                                                (Maybe.map
                                                    (\w ->
                                                        { w
                                                            | x = position.x
                                                            , y = position.y
                                                            , height = position.height
                                                            , width = position.width
                                                        }
                                                    )
                                                )
                                                result
                                        )
                                        model.widgets
                                        newPositions

                                Nothing ->
                                    model.widgets
                    in
                    ( { model
                        | dashboard = Just updatedDashboard
                        , widgets = updatedWidgets
                      }
                    , Cmd.map DashboardMsg cmd
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (List.filterMap identity
            [ Maybe.map (Dashboard.subscriptions >> Sub.map DashboardMsg) model.dashboard ]
        )


view : Model -> Document Msg
view model =
    { title = "Dashboard Demo"
    , body =
        case model.dashboard of
            Nothing ->
                [ text "" ]

            Just dashboard ->
                [ Html.div
                    [ style "width" "100%"
                    , style "height" "100%"
                    , style "background-color" "white"
                    ]
                    [ Html.div
                        []
                        [ Dashboard.view dashboardConfig
                            dashboard
                            (toWidgets model.widgets)
                            ()
                        ]
                    ]
                ]
    }


dashboardConfig :
    { cellSize : Int
    , columnCount : Int
    , gridGap : Int
    , marginBottom : Int
    , marginLeft : Int
    , marginRight : Int
    , marginTop : Int
    , toMsg : Dashboard.Msg -> Msg
    }
dashboardConfig =
    { cellSize = 64
    , columnCount = 20
    , gridGap = 20
    , toMsg = DashboardMsg
    , marginTop = 20
    , marginRight = 50
    , marginLeft = 40
    , marginBottom = 100
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
