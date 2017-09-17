module Dashboard.Internal.AsciiGrid exposing (print, toString)

import Array exposing (Array)


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


type alias Grid a =
    { a | w : Int, h : Int }


type alias Rect a =
    { a | id : Char, x : Int, y : Int, w : Int, h : Int }


type alias Canvas =
    Array (Array Char)



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


setCell : Char -> ( Int, Int ) -> Canvas -> Canvas
setCell value ( x, y ) canvas =
    Array.get y canvas |> Maybe.map (\row -> Array.set y (Array.set x value row) canvas) |> Maybe.withDefault canvas


toLines : Grid a -> List (Rect b) -> List String
toLines grid rects =
    let
        canvas : Canvas
        canvas =
            Array.repeat grid.h (String.repeat grid.w "□" |> String.toList |> Array.fromList)

        blitRect ( i, rect ) canvas =
            let
                xs =
                    List.range rect.x (rect.x + rect.w - 1)

                ys =
                    List.range rect.y (rect.y + rect.h - 1)

                coords =
                    List.map (\y -> List.map (\x -> ( x, y )) xs) ys
                        |> List.concat
            in
            List.foldl (setCell rect.id) canvas coords
    in
    List.foldl blitRect canvas (List.indexedMap (,) rects) |> canvasToLines


canvasToLines : Canvas -> List String
canvasToLines canvas =
    canvas
        |> Array.map (Array.toList >> String.fromList)
        |> Array.toList



-- [ "11111 222 3"
-- , "11111 222 3"
-- , " 44 555    "
-- , " 44 555    "
-- ]


toString : Grid a -> List (Rect b) -> String
toString grid rects =
    String.join "\n" (toLines grid rects)


print : Grid a -> List (Rect b) -> ()
print grid rects =
    toLines grid rects |> List.reverse |> always ()
