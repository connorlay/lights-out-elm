port module LightsOut exposing (..)

import Html exposing (..)
import Html.Events exposing (..)

main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { light : Int }

model : Model
model =
  Model 0

-- UPDATE

type Msg = Toggle

update: Msg -> Model -> Model
update msg model =
  case msg of
    Toggle ->
      if model.light == 1 then
         { model | light = 0 }
      else
         { model | light = 1 }

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    button [onClick Toggle] [text <| displayLight model.light]
  ]

displayLight : Int -> String
displayLight state =
  if state == 0 then "Off" else "On!"
