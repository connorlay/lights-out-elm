port module LightsOut exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

main =
  beginnerProgram { model = model 4, view = view, update = update }

-- MODEL

type alias Model =
  { grid : Array (Array Bool) }

model : Int -> Model
model n =
  {
    grid = False
     |> Array.repeat n
     |> Array.repeat n
  }

-- UPDATE

type alias Msg =
  (Int, Int)

update : Msg -> Model -> Model
update msg model =
  model

-- VIEW

view : Model -> Html Msg
view model =
  text "Hello World"
