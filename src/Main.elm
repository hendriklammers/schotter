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
    ( Int, Int )


type alias Square =
    { size : Int
    , position : Position
    , rotation : Int
    }



-- UPDATE


type Msg
    = RandomNumbers (List Float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomNumbers randoms ->
            ( { model | squares = createSquares model randoms }, Cmd.none )


randomGenerator : Int -> Random.Generator (List Float)
randomGenerator amount =
    Random.list amount (Random.float 0 1)


createSquares : Model -> List Float -> List Square
createSquares model randoms =
    List.map2 (createSquare model)
        (List.range 0 (model.rows * model.columns - 1))
        randoms


createSquare : Model -> Int -> Float -> Square
createSquare { rows, columns, size } index random =
    let
        row =
            index // columns * 5

        damp =
            toFloat rows / toFloat columns ^ 1.5

        offset =
            floor ((random * 2 - 1) * toFloat row * 0.3 * damp)

        position =
            ( modBy columns index * size + offset
            , (index // columns) * size + offset
            )
    in
    Square size position offset



-- VIEW


viewSquare : Square -> Svg Msg
viewSquare { size, position, rotation } =
    let
        ( posX, posY ) =
            position
    in
    rect
        [ x (String.fromInt posX)
        , y (String.fromInt posY)
        , width (String.fromInt size)
        , height (String.fromInt size)
        , fill "none"
        , stroke "black"
        , strokeWidth "2"
        , transform
            ("rotate("
                ++ String.fromInt rotation
                ++ " "
                ++ String.fromInt (posX + size // 2)
                ++ " "
                ++ String.fromInt (posY + size // 2)
                ++ ")"
            )
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
