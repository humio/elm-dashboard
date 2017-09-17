module Example exposing (..)

import Dashboard exposing (Dashboard, Widget(Widget))
import Expect exposing (..)
import Html
import Test exposing (..)


config12 : Dashboard.Config data msg
config12 =
    Dashboard.config
        { columnCount = 12
        , cellSize = 50
        , margin = 30
        , toWidgetContent = \_ -> Html.div [] []
        }


w1 : Widget
w1 =
    Dashboard.widget { id = "x", x = 0, height = 2, width = 2 }


w2 : Widget
w2 =
    Dashboard.widget { id = "y", x = 1, height = 1, width = 3 }


w3 : Widget
w3 =
    Dashboard.widget { id = "z", x = 5, height = 11, width = 2 }


(Dashboard.Widget w1p) =
    w1
(Dashboard.Widget w2p) =
    w2
(Dashboard.Widget w3p) =
    w3


size : number
size =
    50


suite : Test
suite =
    describe "Dashboard"
        [ test "empty" <|
            \_ ->
                Expect.equalLists [] (Dashboard.rects config12 [])
        , test "single widget" <|
            \_ ->
                Expect.equalLists
                    [ { w = w1p.w * size
                      , h = w1p.h * size
                      , x = w1p.x * size
                      , y = 0
                      }
                    ]
                    (Dashboard.rects config12 [ w1 ])
        , test "stacking" <|
            \_ ->
                Dashboard.rects config12 [ w1, w2, w3 ]
                    |> Expect.equalLists
                        [ { w = w1p.w * size
                          , h = w1p.h * size
                          , x = 0
                          , y = 0
                          }
                        , { w = w2p.w * size
                          , h = w2p.h * size
                          , x = w2p.x * size
                          , y = w1p.h * size
                          }
                        , { w = w3p.w * size
                          , h = w3p.h * size
                          , x = w3p.x * size
                          , y = 0
                          }
                        ]
        , test "no change for move if id not found" <|
            \_ ->
                let
                    w1 =
                        Dashboard.widget { id = "a", x = 1, height = 3, width = 2 }

                    w2 =
                        Dashboard.widget { id = "b", x = 2, height = 2, width = 3 }
                in
                assertOperation
                    "MOVE 'xyz' -> (0, 2)"
                    config12
                    (Dashboard.move "xyz" { x = 0, y = 2 })
                    [ w1, w2 ]
                    [ w1, w2 ]
        , test "move widget" <|
            \_ ->
                let
                    w1 =
                        Dashboard.widget { id = "a", x = 1, height = 3, width = 2 }

                    w2 =
                        Dashboard.widget { id = "b", x = 2, height = 2, width = 3 }

                    (Widget w1p) =
                        w1

                    (Widget w2p) =
                        w2

                    dashboard =
                        [ w1, w2 ]

                    expected =
                        [ Widget { w2p | x = 0 }, w1 ]
                in
                assertOperation
                    "MOVE 'b' -> (0, 2)"
                    config12
                    (Dashboard.move "b" { x = 0, y = 2 })
                    dashboard
                    expected
        , test "move widget where one if unaffected" <|
            \_ ->
                let
                    w1 =
                        Dashboard.widget { id = "x", x = 1, height = 3, width = 2 }

                    w2 =
                        Dashboard.widget { id = "y", x = 2, height = 2, width = 3 }

                    (Widget w1p) =
                        w1

                    (Widget w2p) =
                        w2

                    dashboard =
                        [ w1, w2 ]

                    expected =
                        [ Widget { w2p | x = 0 }, w1 ]
                in
                assertOperation
                    "MOVE '' -> (0, 2)"
                    config12
                    (Dashboard.move "y" { x = 0, y = 2 })
                    dashboard
                    expected
        , test "move example 2" <|
            \_ ->
                let
                    dashboard =
                        [ Widget { id = "A", w = 3, h = 4, x = 1 }
                        , Widget { id = "B", w = 3, h = 4, x = 3 }
                        , Widget { id = "C", w = 3, h = 4, x = 4 }
                        , Widget { id = "D", w = 3, h = 2, x = 8 }
                        ]

                    expected =
                        [ Widget { id = "A", w = 3, h = 4, x = 1 }
                        , Widget { id = "D", w = 3, h = 2, x = 5 }
                        , Widget { id = "B", w = 3, h = 4, x = 3 }
                        , Widget { id = "C", w = 3, h = 4, x = 4 }
                        ]
                in
                assertOperation
                    " Move D -> (5, 0)"
                    config12
                    (Dashboard.move "D" { x = 5, y = 0 })
                    dashboard
                    expected
        ]


assertOperation : String -> Dashboard.Config data msg -> (Dashboard.Config data msg -> Dashboard -> Dashboard) -> Dashboard -> Dashboard -> Expectation
assertOperation operation config change input expected =
    change config input |> expectEqualDashboards config operation input expected


expectEqualDashboards :
    Dashboard.Config data msg
    -> String
    -> Dashboard
    -> Dashboard
    -> Dashboard
    -> Expectation
expectEqualDashboards config operation original expected actual =
    if actual == expected then
        Expect.pass
    else
        Expect.fail <|
            "Original Dashboard\n\n"
                ++ Dashboard.toString config original
                ++ "\n\n"
                ++ toString original
                ++ "\n\nChange: "
                ++ operation
                ++ "\n\nOutcome:\n\n"
                ++ toString actual
                ++ "\n\n"
                ++ Dashboard.toString config actual
                ++ "\n\nExpected:\n\n"
                ++ Dashboard.toString config expected
