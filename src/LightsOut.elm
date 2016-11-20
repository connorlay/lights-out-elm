port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (get, set, Array, toIndexedList)
import Set exposing (foldl, fromList, Set)
import Maybe exposing (withDefault)

main =
  beginnerProgram { model = model 4, view = view, update = update }

-- MODEL

type alias Grid =
  Array (Array Bool)

type alias Model =
  { grid : Grid }

model : Int -> Model
model n =
  { grid = False |> Array.repeat n |> Array.repeat n }

-- UPDATE

type alias Coord =
  (Int, Int)

type alias Msg =
  Coord

update : Msg -> Model -> Model
update msg model =
  case msg of
    (_, _) ->
      { model | grid = updateGrid model.grid (neighbors msg) }

updateGrid : Grid -> Set Coord -> Grid
updateGrid grid coords =
  foldl toggle grid coords

neighbors : Coord -> Set Coord
neighbors (r1, c1) =
  [ (0, 0), (1, 0), (-1, 0), (0, 1), (0, -1) ]
  |> fromList
  |> Set.map (\(r2, c2) -> (r1 + r2, c1 + c2))

toggle : Coord -> Grid -> Grid
toggle (r, c) grid =
  let
    toggled = getElement (r, c) grid |> not
    row = grid
          |> get r
          |> withDefault Array.empty
          |> set c toggled
  in
    set r row grid

getElement : Coord -> Grid -> Bool
getElement (r, c) grid =
  grid
  |> get r
  |> withDefault Array.empty
  |> get c
  |> withDefault False

-- VIEW

view : Model -> Html Msg
view model =
  div [] <| asHtml model.grid

asHtml : Grid -> List (Html Msg)
asHtml grid =
  grid
  |> toIndexedList
  |> List.foldr (\row acc -> (rowAsHtml row) :: acc) []

rowAsHtml : (Int, Array Bool) -> Html Msg
rowAsHtml (r, row) =
  row
  |> toIndexedList
  |> List.foldr (\(c, e) acc -> button [onClick (r, c), style [ ("backgroundColor", stateToColor e) ] ] [text "Click Me!"] :: acc) []
  |> div []

stateToColor : Bool -> String
stateToColor state =
  if state then "red" else "gray"
