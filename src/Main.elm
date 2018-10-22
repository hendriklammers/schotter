module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Random
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , height
        , stroke
        , strokeWidth
        , transform
        , viewBox
        , width
        , x
        , y
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { squares = []
            , rows = 22
            , columns = 12
            , size = 30
            }
    in
    ( model
    , (model.rows * model.columns)
        |> randomGenerator
        |> Random.generate RandomNumbers
    )



-- MODEL


type alias Model =
    { squares : List Square
    , rows : Int
    , columns : Int
    , size : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias Square =
    { size : Int
    , position : Position
    , rotation : Int
    }


randomGenerator : Int -> Random.Generator (List Float)
randomGenerator amount =
    Random.list amount (Random.float 0 1)


createSquares : Model -> List Float -> List Square
createSquares { rows, columns, size } randoms =
    let
        square index random =
            let
                row =
                    index // columns * 5

                rotation =
                    floor ((random * 2 - 1) * toFloat row * 0.3 * (toFloat rows / toFloat columns ^ 1.5))

                offset =
                    { x = rotation
                    , y = rotation
                    }

                position =
                    { x = modBy columns index * size
                    , y = (index // columns) * size
                    }
            in
            Square size (add position offset) rotation
    in
    List.map2 square
        (List.range 0 (rows - 1))
        randoms


add : Position -> Position -> Position
add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }



-- UPDATE


type Msg
    = RandomNumbers (List Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomNumbers randoms ->
            ( { model | squares = createSquares model randoms }, Cmd.none )



-- VIEW


viewSquare : Square -> Svg Msg
viewSquare { size, position, rotation } =
    let
        tf =
            "rotate("
                ++ String.fromInt rotation
                ++ " "
                ++ String.fromInt (position.x + size // 2)
                ++ " "
                ++ String.fromInt (position.y + size // 2)
                ++ ")"
    in
    rect
        [ x (String.fromInt position.x)
        , y (String.fromInt position.y)
        , width (String.fromInt size)
        , height (String.fromInt size)
        , fill "none"
        , stroke "black"
        , strokeWidth "2"
        , transform tf
        ]
        []


view : Model -> Html Msg
view { squares, rows, columns, size } =
    let
        w =
            columns * size + size * 2 |> String.fromInt

        h =
            rows * size + size |> String.fromInt
    in
    svg
        [ width w
        , height h
        , viewBox ("0 0 " ++ w ++ " " ++ h)
        ]
        [ g
            [ transform ("translate(" ++ String.fromInt size ++ " 0)") ]
            (List.map viewSquare squares)
        ]
