port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)

main =
  program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type alias Grid =
  Array (Array Bool)

type alias Model =
  { grid : Grid }

model : Int -> Model
model n =
  { grid = False |> Array.repeat n |> Array.repeat n }

init : (Model, Cmd Msg)
init =
  (model 4, Cmd.none)

-- UPDATE

type alias Coord =
  (Int, Int)

type alias Msg =
  Coord

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    (_, _) ->
      ( { model | grid = updateGrid model.grid (neighbors msg) }, Cmd.none )

updateGrid : Grid -> Set Coord -> Grid
updateGrid grid coords =
  Set.foldl toggleCell grid coords

neighbors : Coord -> Set Coord
neighbors (row, col) =
  [ (0, 0), (1, 0), (-1, 0), (0, 1), (0, -1) ]
  |> Set.fromList
  |> Set.map (\(row_, col_) -> (row + row_, col + col_))

toggleCell : Coord -> Grid -> Grid
toggleCell (row, col) grid =
  let
    toggleCelld = getElement (row, col) grid |> not
    cells = grid
          |> get row
          |> withDefault Array.empty
          |> set col toggleCelld
  in
    set row cells grid

getElement : Coord -> Grid -> Bool
getElement (row, col) grid =
  grid
  |> get row
  |> withDefault Array.empty
  |> get col
  |> withDefault False

-- VIEW

view : Model -> Html Msg
view model =
  div [] <| gridAsHtml model.grid

gridAsHtml : Grid -> List (Html Msg)
gridAsHtml grid =
  grid
  |> toIndexedList
  |> List.foldr (\row acc -> (rowAsHtml row) :: acc) []

rowAsHtml : (Int, Array Bool) -> Html Msg
rowAsHtml (row, cells) =
  cells
  |> toIndexedList
  |> List.foldr (\(col, state) acc -> cellAsHtml (row, col) state :: acc) []
  |> div []

cellAsHtml : Coord -> Bool -> Html Msg
cellAsHtml coord state =
  button [
    onClick coord,
    style [ ("backgroundColor", if state then "red" else "gray") ]
  ] [ text "Click Me!" ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
