port module LightsOut exposing (Coord, model, neighbors)

import Html exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Set exposing (..)

main =
  beginnerProgram { model = model 4, view = view, update = update }

-- MODEL

type alias Grid =
  Array (Array Bool)

type alias Model =
  { grid : Grid }

model : Int -> Model
model n =
  {
    grid = False
     |> Array.repeat n
     |> Array.repeat n
  }

-- UPDATE

type alias Coord =
  (Int, Int)

type alias Msg =
  Coord

update : Msg -> Model -> Model
update msg model =
  model

neighbors : Coord -> Set Coord
neighbors (r1, c1) =
  [ (1, 0), (-1, 0), (0, 1), (0, -1) ]
  |> Set.fromList
  |> Set.map (\(r2, c2) -> (r1 + r2, c1 + c2))

-- VIEW

view : Model -> Html Msg
view model =
  text "Hello World"
