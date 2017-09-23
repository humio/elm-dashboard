module Dashboard.Internal.Data exposing (..)

import Html exposing (Html)
import Mouse


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


type alias Rect =
    { w : Int
    , h : Int
    , x : Int
    , y : Int
    }


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
