module DashboardTest exposing (..)

import Dashboard.Internal.Layout as Layout
import Expect exposing (..)
import Test exposing (..)


type Msg
    = Noop


defaultCellWidth : Int
defaultCellWidth =
    10


defaultConfig : Layout.Config {}
defaultConfig =
    { columnCount = 3
    , cellSize = defaultCellWidth
    , gridGap = 2
    , marginTop = 0
    , marginRight = 0
    , marginBottom = 0
    , marginLeft = 0
    }


dashboardConfig : Layout.Config {}
dashboardConfig =
    { cellSize = 64
    , columnCount = 10
    , gridGap = 10
    , marginTop = 0
    , marginRight = 0
    , marginBottom = 0
    , marginLeft = 0
    }


suite : Test
suite =
    describe "Dashboard"
        [ test "isOverlapping" <|
            \_ ->
                Expect.true "overlap"
                    (Layout.isOverlapping { x = 0, y = 0, height = 2, width = 3 }
                        { x = 0, y = 0, height = 2, width = 3 }
                    )
        , test "not isOverlapping" <|
            \_ ->
                Expect.false "not overlap"
                    (Layout.isOverlapping { x = 0, y = 0, height = 2, width = 3 }
                        { x = 3, y = 0, height = 2, width = 3 }
                    )
        , test "out of bounds" <|
            \_ ->
                let
                    input =
                        [ { id = "A", x = -1, y = -2, height = 2, width = 3 }
                        , { id = "B", x = 0, y = 0, height = 2, width = 3 }
                        ]

                    output =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 3 }
                        , { id = "B", x = 0, y = 2, height = 2, width = 3 }
                        ]
                in
                Expect.equalLists (Layout.correct dashboardConfig input) output
        , test "correct vertical" <|
            \_ ->
                let
                    input =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 3 }
                        , { id = "B", x = 0, y = 0, height = 2, width = 3 }
                        ]

                    output =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 3 }
                        , { id = "B", x = 0, y = 2, height = 2, width = 3 }
                        ]
                in
                Expect.equalLists (Layout.correct dashboardConfig input) output
        , test "correct no change" <|
            \_ ->
                let
                    input =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 3 }
                        , { id = "A", x = 3, y = 0, height = 2, width = 3 }
                        ]
                in
                Expect.equalLists (Layout.correct dashboardConfig input) input
        , test "different x" <|
            \_ ->
                let
                    input =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 1 }
                        , { id = "B", x = 0, y = 0, height = 2, width = 2 }
                        , { id = "C", x = 1, y = 2, height = 2, width = 2 }
                        ]

                    output =
                        [ { id = "A", x = 0, y = 0, height = 2, width = 1 }
                        , { id = "B", x = 0, y = 2, height = 2, width = 2 }
                        , { id = "C", x = 1, y = 4, height = 2, width = 2 }
                        ]
                in
                Expect.equalLists (Layout.correct dashboardConfig input) output
        , test "move to gap, stay put" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 7, y = 5 }

                    mousePosition =
                        { x = 9, y = 5 }

                    widget =
                        { x = 0, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal widget
        , test "move before center of next cell, stay put" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 7, y = 5 }

                    mousePosition =
                        { x = round <| toFloat mouseOrigin.x + toFloat config.gridGap + toFloat config.cellSize / 2 - 1, y = 5 }

                    widget =
                        { x = 0, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal widget
        , test "move to center of next cell, increase" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 7, y = 5 }

                    mousePosition =
                        { x = round <| toFloat mouseOrigin.x + toFloat config.gridGap + toFloat config.cellSize / 2
                        , y = 5
                        }

                    widget =
                        { x = 0, y = 0, width = 1, height = 1 }

                    newWidget =
                        { x = 1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal newWidget
        , test "move past center of next cell, increase only 1" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 7, y = 5 }

                    mousePosition =
                        { x = round <| toFloat mouseOrigin.x + toFloat config.gridGap + toFloat config.cellSize / 2 + 1
                        , y = 5
                        }

                    widget =
                        { x = 0, y = 0, width = 1, height = 1 }

                    newWidget =
                        { x = 1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal newWidget
        , test "move before center of second cell, increase only 1" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 7, y = 5 }

                    firstStep =
                        toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = round <| toFloat mouseOrigin.x + firstStep + toFloat config.cellSize - 1
                        , y = 5
                        }

                    widget =
                        { x = 0, y = 0, width = 1, height = 1 }

                    newWidget =
                        { x = 1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal newWidget
        , test "move left, decrease 1" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 17, y = 5 }

                    firstStep =
                        toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = round <| toFloat mouseOrigin.x - firstStep
                        , y = 5
                        }

                    widget =
                        { x = 1, y = 0, width = 2, height = 3 }

                    diff =
                        { x = -1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal diff
        , test "cant move below zero" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 17, y = 5 }

                    firstStep =
                        toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = -10000
                        , y = 5
                        }

                    widget =
                        { x = 1, y = 0, width = 2, height = 3 }

                    diff =
                        { x = -1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal diff
        , test "can't move past max column" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 17, y = 5 }

                    firstStep =
                        toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = 10000
                        , y = 5
                        }

                    widget =
                        { x = 1, y = 0, width = 1, height = 3 }

                    diff =
                        { x = 1, y = 0, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal diff
        , test "move one y at center of next cell" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 2, y = 2 }

                    firstStep =
                        round <| toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = mouseOrigin.x
                        , y = mouseOrigin.y + firstStep
                        }

                    widget =
                        { x = 0, y = 0, width = 1, height = 1 }

                    diff =
                        { x = 0, y = 1, width = 0, height = 0 }
                in
                Layout.move config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal diff
        , test "resize one y on SouthEast at center of next cell" <|
            \_ ->
                let
                    config =
                        defaultConfig

                    mouseOrigin =
                        { x = 2, y = 2 }

                    firstStep =
                        round <| toFloat config.gridGap + toFloat config.cellSize / 2

                    mousePosition =
                        { x = mouseOrigin.x
                        , y = mouseOrigin.y + firstStep
                        }

                    widget =
                        { x = 0, y = 0, width = 1, height = 1 }

                    diff =
                        { x = 0, y = 0, width = 0, height = 1 }
                in
                Layout.resize Layout.SE config defaultCellWidth mouseOrigin mousePosition widget |> Expect.equal diff
        ]
