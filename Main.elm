module Main exposing (..)

import Dashboard
import Dashboard.Internal.Data
import Dashboard.Internal.Layout
import Html exposing (Html, program)
import Html.Attributes exposing (style)


type alias Model =
    { dashboard : Dashboard.Model }


type Msg
    = NoOp
    | DashboardMsg Dashboard.Msg


init : ( Model, Cmd Msg )
init =
    ( { dashboard =
            Dashboard.initWithWidgets
                [ Dashboard.Internal.Layout.widget { id = "1", x = 0, width = 2, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "2", x = 2, width = 2, height = 2 }
                , Dashboard.Internal.Layout.widget { id = "3", x = 4, width = 1, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "4", x = 5, width = 1, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "5", x = 0, width = 1, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "6", x = 1, width = 1, height = 2 }
                , Dashboard.Internal.Layout.widget { id = "7", x = 4, width = 2, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "8", x = 0, width = 1, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "9", x = 2, width = 2, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "10", x = 4, width = 1, height = 1 }
                , Dashboard.Internal.Layout.widget { id = "11", x = 5, width = 1, height = 1 }
                ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        DashboardMsg msg ->
            let
                ( updatedDashboard, cmd ) =
                    Dashboard.update msg dashboardConfig model.dashboard
            in
            { model | dashboard = updatedDashboard } ! [ Cmd.map DashboardMsg cmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DashboardMsg (Dashboard.subscriptions model.dashboard)


dashboardConfig : Dashboard.Internal.Data.Config Dashboard.Internal.Data.Widget msg
dashboardConfig =
    Dashboard.config
        { columnCount = 6
        , cellSize = 120
        , margin = 20
        , toWidgetContent = \(Dashboard.Internal.Data.Widget w) -> Html.text w.id
        }


view : Model -> Html Msg
view model =
    let
        (Dashboard.Internal.Data.Config config) =
            dashboardConfig
    in
    Html.div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", "rgb(18, 31, 83)" )
            , ( "padding", "1px" )
            , ( "color", "#ffffff" )
            ]
        ]
        [ Html.div
            [ style
                [ ( "width", (toString <| config.columnCount * config.cellSize) ++ "px" )
                , ( "height", "1000px" )
                , ( "margin", "50px 100px" )
                ]
            , Html.Attributes.id "p"
            ]
            [ Html.div
                [ Html.Attributes.style
                    [ ( "position", "absolute" )
                    , ( "top", "10px" )
                    , ( "right", "10px" )
                    , ( "max-width", "500px" )
                    ]
                ]
                [ case model.dashboard.dragState of
                    Dashboard.Internal.Data.NotDragging ->
                        Html.text ""

                    Dashboard.Internal.Data.Dragging info ->
                        Html.div []
                            [ Html.text <| toString <| Dashboard.Internal.Layout.pointToCoords dashboardConfig info.screenPosition
                            , Html.div [] [ Html.text <| "Screen Position: " ++ toString info.screenPosition ]
                            , Html.div [] [ Html.text <| "Point In Widget: " ++ toString info.parentOffset ]
                            ]
                , Html.div [] [ Html.text <| toString model.dashboard ]
                ]
            , Html.map DashboardMsg <|
                Dashboard.view dashboardConfig model.dashboard
            ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
