port module Grid.Update exposing (..)

import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Grid.Model exposing (..)
import Grid.Message exposing (..)

type alias Msg = Grid.Message.Msg
type alias Model = Grid.Model.Model
type alias Grid = Grid.Model.Grid

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGrid bools ->
            ( { model | grid = createGrid bools, moves = 0 }, Cmd.none )

        ToggleCell coord ->
            ( { model
                | grid = updateGrid model.grid (neighbors coord)
                , moves = model.moves + 1
              }
            , Cmd.none
            )

createGrid : List Bool -> Grid
createGrid bools =
    bools
        |> chunk (dimension bools)
        |> as2x2Array


dimension : List a -> Int
dimension list =
    list
        |> List.length
        |> toFloat
        |> sqrt
        |> ceiling


chunk : Int -> List a -> List (List a)
chunk k xs =
    if k == 0 then
        [ [] ]
    else if k < 0 then
        []
    else if List.length xs > k then
        List.take k xs :: chunk k (List.drop k xs)
    else
        [ xs ]


as2x2Array : List (List Bool) -> Array (Array Bool)
as2x2Array list =
    list
        |> List.map (Array.fromList)
        |> Array.fromList


updateGrid : Grid -> Set ( Int, Int ) -> Grid
updateGrid grid coords =
    Set.foldl toggleCell grid coords


neighbors : ( Int, Int ) -> Set ( Int, Int )
neighbors ( row, col ) =
    [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]
        |> Set.fromList
        |> Set.map (\( row_, col_ ) -> ( row + row_, col + col_ ))


toggleCell : ( Int, Int ) -> Grid -> Grid
toggleCell ( row, col ) grid =
    let
        toggledCell =
            grid
                |> get2x2 ( row, col )
                |> not

        cells =
            grid
                |> Array.get row
                |> withDefault Array.empty
                |> Array.set col toggledCell
    in
        Array.set row cells grid


get2x2 : ( Int, Int ) -> Grid -> Bool
get2x2 ( row, col ) grid =
    grid
        |> Array.get row
        |> withDefault Array.empty
        |> Array.get col
        |> withDefault False


allOff : Grid -> Bool
allOff grid =
    grid
        |> Array.toList
        |> List.all (\row -> row |> Array.toList |> List.all not)

