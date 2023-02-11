module Main exposing (Model, init, main, update, view)

-- Html.Events.Extra.Mouse
-- https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/4.0.2/

import Array exposing (Array, toList)
import Browser
import Element exposing (px, rgb255, rgba255)
import Element.Background
import Element.Border
import Element.Events exposing (onClick)
import Element.Font
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import List
import List.Extra
import Platform.Cmd exposing (Cmd)
import Random
import Random.Array
import Svg
import Svg.Attributes
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Board =
    Array (Array Cell)


type CellType
    = Revealed
    | Hidden
    | Flagged


type alias Cell =
    { celltype : CellType
    , mine : Bool
    }


type Msg
    = Click Int Int Mouse.Button
    | Reset
    | Seed Random.Seed


type alias Model =
    { boardWidth : Int
    , boardHeight : Int
    , board : Board
    , gameOver : Bool
    , seed : Random.Seed
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardWidth = 30
      , boardHeight = 20
      , board = Array.repeat 25 (Array.repeat 16 { celltype = Hidden, mine = False })
      , gameOver = False
      , seed = Random.initialSeed 15
      }
    , Random.generate Seed Random.independentSeed
    )


initBoard : Model -> Random.Seed -> Board
initBoard model seed =
    let
        bool =
            Random.map (\n -> n < 16) (Random.int 1 100)

        feild b =
            { celltype = Hidden, mine = b }

        ( randomList, seeds ) =
            Random.step
                (Random.Array.array (model.boardWidth * model.boardHeight) bool)
                seed

        dummyBoard =
            { visible = Hidden, mine = False }
                |> Array.repeat model.boardHeight
                |> Array.repeat model.boardWidth
    in
    Array.indexedMap
        (\x column ->
            Array.indexedMap
                (\y ele ->
                    randomList
                        |> Array.get (y * model.boardWidth + x)
                        |> Maybe.withDefault False
                        |> feild
                )
                column
        )
        dummyBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model
                | board = initBoard model (Random.initialSeed 0)
                , gameOver = False
              }
            , Random.generate Seed Random.independentSeed
            )

        Click x y button ->
            ( clickCell model x y button
            , Cmd.none
            )

        Seed seed ->
            ( { model
                | seed = seed
                , board = initBoard model seed
              }
            , Cmd.none
            )


clickCell : Model -> Int -> Int -> Mouse.Button -> Model
clickCell model xx yy button =
    let
        x =
            clamp 0 (model.boardWidth - 1) xx

        y =
            clamp 0 (model.boardHeight - 1) yy

        cell =
            model.board
                |> Array.get x
                |> Maybe.withDefault Array.empty
                |> Array.get y
                |> Maybe.withDefault { celltype = Hidden, mine = False }

        count =
            countNeighbors x y model.board

        newCell =
            case ( button, cell.celltype, model.gameOver ) of
                ( Mouse.MainButton, Hidden, False ) ->
                    { cell | celltype = Revealed }

                ( Mouse.SecondButton, Hidden, False ) ->
                    { cell | celltype = Flagged }

                ( Mouse.SecondButton, Flagged, False ) ->
                    { cell | celltype = Hidden }

                _ ->
                    cell

        -- { cell | visible = Visible }
        newBoard =
            model.board
                |> Array.set x
                    (model.board
                        |> Array.get x
                        |> Maybe.withDefault Array.empty
                        |> Array.set y newCell
                    )

        newModel =
            if newCell.mine && newCell.celltype == Revealed then
                { model | gameOver = True, board = newBoard }

            else
                { model | board = newBoard }
    in
    if count == 0 && cell.celltype == Hidden && newCell.celltype == Revealed then
        List.Extra.cartesianProduct [ List.range -1 1, List.range -1 1 ]
            |> List.Extra.scanl
                (\list m ->
                    let
                        ( dx, dy ) =
                            Maybe.withDefault ( 0, 0 ) <| listToTuple2 list
                    in
                    clickCell m (x + dx) (y + dy) Mouse.MainButton
                )
                newModel
            |> List.Extra.last
            |> Maybe.withDefault newModel

    else
        newModel


listToTuple2 : List a -> Maybe ( a, a )
listToTuple2 list =
    case list of
        [ a, b, c ] ->
            Nothing

        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getWithOffset : Int -> Int -> Int -> Int -> Board -> Cell
getWithOffset x dx y dy board =
    board
        |> Array.get (x + dx)
        |> Maybe.withDefault Array.empty
        |> Array.get (y + dy)
        |> Maybe.withDefault { celltype = Hidden, mine = False }


countNeighbors : Int -> Int -> Board -> Int
countNeighbors x y board =
    List.map
        (\dx ->
            List.map
                (\dy ->
                    if (getWithOffset x dx y dy board).mine then
                        1

                    else
                        0
                )
                (List.range -1 1)
                |> List.sum
        )
        (List.range -1 1)
        |> List.sum


viewCell : Model -> Cell -> Int -> Int -> Element.Element Msg
viewCell model cell x y =
    let
        count =
            countNeighbors x y model.board

        fontColor countOfNeighbors =
            Element.Font.color
                (case countOfNeighbors of
                    1 ->
                        rgba255 0 0 255 1.0

                    2 ->
                        rgba255 0 127 0 1.0

                    3 ->
                        rgba255 255 0 0 1.0

                    4 ->
                        rgba255 200 0 127 1.0

                    5 ->
                        rgba255 127 0 0 1.0

                    6 ->
                        rgba255 0 127 127 1.0

                    7 ->
                        rgba255 0 0 0 1.0

                    8 ->
                        rgba255 127 127 127 1.0

                    _ ->
                        rgba255 0 0 0 1.0
                )

        displayCell =
            case ( cell.celltype, cell.mine, count ) of
                ( Flagged, _, _ ) ->
                    svgFlag

                ( Revealed, True, _ ) ->
                    -- ( _, True, _ ) ->
                    -- For Debug purposes
                    svgMine

                ( Revealed, False, 0 ) ->
                    Element.text ""

                ( Revealed, False, _ ) ->
                    Element.el
                        [ fontColor count
                        ]
                        (String.fromInt count
                            |> Element.text
                        )

                _ ->
                    Element.text ""

        color =
            case ( cell.celltype, cell.mine ) of
                ( Revealed, True ) ->
                    rgba255 255 0 0 1.0

                ( Revealed, False ) ->
                    rgba255 255 255 255 1.0

                ( Hidden, _ ) ->
                    rgba255 100 100 100 1.0

                ( Flagged, _ ) ->
                    rgba255 200 200 200 1.0
    in
    Element.el
        [ Element.htmlAttribute <| Mouse.onClick (\event -> Click x y event.button)
        , Element.htmlAttribute <| Mouse.onContextMenu (\event -> Click x y event.button)
        , Element.width <| px 30
        , Element.height <| px 30
        , Element.Background.color color
        , Element.Border.rounded 2
        , Element.centerX
        , Element.centerY
        , Element.Border.innerGlow (rgba255 0 0 0 0.15) 2.5

        -- , Element.Border.widthEach { top = 2, right = 2, bottom = 2, left = 2 }
        -- , Element.Border.color (rgba255 0 0 0 0.2)
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
            displayCell


svgFlag : Element.Element Msg
svgFlag =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "28"
            , Svg.Attributes.height "28"
            ]
            [ Svg.rect
                [ Svg.Attributes.width "2"
                , Svg.Attributes.height "18"
                , Svg.Attributes.rx "0"
                , Svg.Attributes.x "11"
                , Svg.Attributes.y "10"

                -- , Svg.Attributes.fill <| color
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "14"
                , Svg.Attributes.height "3"
                , Svg.Attributes.rx "0"
                , Svg.Attributes.x "6"
                , Svg.Attributes.y "26"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "10"
                , Svg.Attributes.height "3"
                , Svg.Attributes.rx "0"
                , Svg.Attributes.x "8"
                , Svg.Attributes.y "24"
                ]
                []
            , Svg.polygon
                [ Svg.Attributes.points "10.5,7 10.5,18  14,19 18,17  22,18 22,7  18,6 14,8 "
                , Svg.Attributes.fill <| "red"
                ]
                []
            ]


svgMine : Element.Element Msg
svgMine =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "28"
            , Svg.Attributes.height "28"
            ]
            [ Svg.circle
                [ Svg.Attributes.r "9"
                , Svg.Attributes.cx "14"
                , Svg.Attributes.cy "14"

                -- , Svg.Attributes.fill <| color
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "2"
                , Svg.Attributes.height "24"
                , Svg.Attributes.x "13"
                , Svg.Attributes.y "2"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "24"
                , Svg.Attributes.height "2"
                , Svg.Attributes.x "2"
                , Svg.Attributes.y "13"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "22"
                , Svg.Attributes.height "2"
                , Svg.Attributes.x "9"
                , Svg.Attributes.y "-1"
                , Svg.Attributes.transform "rotate(45)"
                ]
                []
            , Svg.rect
                [ Svg.Attributes.width "22"
                , Svg.Attributes.height "2"
                , Svg.Attributes.x "-11"
                , Svg.Attributes.y "19"
                , Svg.Attributes.transform "rotate(-45)"
                ]
                []
            , Svg.polygon
                [ Svg.Attributes.points "13,8 13,10 11,11 10,13 8,13 9,9 "
                , Svg.Attributes.fill <| "white"
                ]
                []
            ]


viewBoard : Model -> Element.Element Msg
viewBoard model =
    model.board
        |> Array.indexedMap
            (\x column ->
                column
                    |> Array.indexedMap (\y ele -> viewCell model ele x y)
                    |> Array.toList
                    |> Element.column
                        [ Element.spacing 1
                        ]
            )
        |> Array.toList
        |> Element.row
            [ Element.spacing 1
            , Element.Background.color <| rgb255 200 200 200
            ]


viewResetButton : Model -> Element.Element Msg
viewResetButton model =
    Element.el
        [ Element.Events.onClick Reset
        , Element.Border.width 1
        , Element.paddingXY 5 5
        ]
        (Element.text
            "Reset"
        )


view : Model -> Html Msg
view model =
    Html.Lazy.lazy
        (Element.layout [])
        (Element.column [ Element.centerX, Element.centerY ]
            [ viewBoard model
            , viewResetButton model
            ]
        )
