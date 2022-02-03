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


type VisibleCell
    = Visible
    | Hidden
    | Flagged


type alias Cell =
    { visible : VisibleCell
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
      , board = Array.repeat 25 (Array.repeat 16 { visible = Hidden, mine = False })
      , gameOver = False
      , seed = Random.initialSeed 15
      }
    , Random.generate Seed Random.independentSeed
    )


initBoard : Model -> Random.Seed -> Board
initBoard model seed =
    let
        bool =
            Random.map (\n -> n < 20) (Random.int 1 100)

        feild b =
            if b then
                { visible = Hidden, mine = True }

            else
                { visible = Hidden, mine = False }

        ( randomList, seeds ) =
            Random.step
                (Random.Array.array (model.boardWidth * model.boardHeight) bool)
                seed

        dummyBoard =
            Array.repeat model.boardWidth (Array.repeat model.boardHeight { visible = Hidden, mine = False })
    in
    dummyBoard
        |> Array.indexedMap
            (\x column ->
                column
                    |> Array.indexedMap
                        (\y ele ->
                            Array.get (y * model.boardWidth + x) randomList
                                |> Maybe.withDefault False
                                |> feild
                        )
            )


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
                |> Maybe.withDefault { visible = Hidden, mine = False }

        count =
            countNeighbors x y model.board

        newCell =
            case ( button, cell.visible, model.gameOver ) of
                ( Mouse.MainButton, Hidden, False ) ->
                    { cell | visible = Visible }

                ( Mouse.SecondButton, Hidden, False ) ->
                    { cell | visible = Flagged }

                ( Mouse.SecondButton, Flagged, False ) ->
                    { cell | visible = Hidden }

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
            if newCell.mine && newCell.visible == Visible then
                { model | gameOver = True, board = newBoard }

            else
                { model | board = newBoard }
    in
    if count == 0 && cell.visible == Hidden && newCell.visible == Visible then
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
        |> Maybe.withDefault { visible = Hidden, mine = False }


countNeighbors : Int -> Int -> Board -> Int
countNeighbors x y board =
    List.range -1 1
        |> List.map
            (\dx ->
                List.range -1 1
                    |> List.map
                        (\dy ->
                            if (getWithOffset x dx y dy board).mine then
                                1

                            else
                                0
                        )
                    |> List.sum
            )
        |> List.sum


viewCell : Model -> Cell -> Int -> Int -> Element.Element Msg
viewCell model cell x y =
    let
        count =
            countNeighbors x y model.board

        displayText =
            case ( cell.visible, cell.mine, count ) of
                ( Visible, True, _ ) ->
                    -- ( _, True, _ ) -> For Debug purposes
                    "X"

                ( Visible, False, 0 ) ->
                    ""

                ( Visible, False, _ ) ->
                    String.fromInt count

                _ ->
                    ""

        color =
            case ( cell.visible, cell.mine ) of
                ( Visible, True ) ->
                    rgba255 255 0 0 1.0

                ( Visible, False ) ->
                    rgba255 255 255 255 1.0

                ( Hidden, _ ) ->
                    rgba255 100 100 100 1.0

                ( Flagged, _ ) ->
                    rgba255 0 200 0 1.0
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
        ]
    <|
        Element.el
            [ Element.centerX
            , Element.centerY
            ]
        <|
            Element.text displayText


svgCell : Model -> Cell -> Int -> Int -> Element.Element Msg
svgCell model cell x y =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "30"
            , Svg.Attributes.height "30"
            ]
            [ Svg.rect
                [ Svg.Attributes.width "30"
                , Svg.Attributes.height "30"
                , Svg.Attributes.rx "5"

                -- , Svg.Attributes.fill <| color
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x "9"
                , Svg.Attributes.y "22"
                ]
                [-- Svg.text displayText
                ]
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
