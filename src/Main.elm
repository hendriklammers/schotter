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
        { init =
            \_ ->
                ( []
                , Random.generate RandomNumbers randomGenerator
                )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    List Square


type alias Position =
    { x : Int
    , y : Int
    }


type alias Square =
    { size : Int
    , position : Position
    , rotation : Int
    }


type alias Dimensions =
    { rows : Int
    , columns : Int
    , size : Int
    }


dimensions : Dimensions
dimensions =
    { columns = 12
    , rows = 22
    , size = 30
    }


randomGenerator : Random.Generator (List Float)
randomGenerator =
    Random.list (dimensions.rows * dimensions.columns) (Random.float 0 1)



-- math.Pow(row/numRows, 1.5)


squares : List Float -> List Square
squares randoms =
    let
        square index random =
            let
                row =
                    index // dimensions.columns * 5

                rotation =
                    floor ((random * 2 - 1) * toFloat row * 0.3 * (toFloat dimensions.rows / toFloat dimensions.columns ^ 1.5))

                offset =
                    { x = rotation
                    , y = rotation
                    }

                position =
                    positionSquare dimensions index
            in
            Square dimensions.size (add position offset) rotation
    in
    List.map2 square
        (List.range 0 (dimensions.columns * dimensions.rows - 1))
        randoms


positionSquare : Dimensions -> Int -> Position
positionSquare { columns, size } index =
    { x = modBy columns index * size
    , y = (index // columns) * size
    }


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
            ( squares randoms, Cmd.none )



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
view model =
    svg
        [ width "420"
        , height "690"
        , viewBox "0 0 420 690"
        ]
        [ g
            [ transform "translate(30 0)" ]
            (List.map viewSquare model)
        ]
