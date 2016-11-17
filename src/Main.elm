port module LightsOut exposing (..)

import Html exposing (..)
import Html.Events exposing (..)

main =
  beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { light : Bool }

model : Model
model =
  Model False

-- UPDATE

type Msg = Toggle

update: Msg -> Model -> Model
update msg model =
  case msg of
    Toggle ->
      if model.light then
         { model | light = False }
      else
         { model | light = True }

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    button [onClick Toggle] [text <| displayLight model.light]
  ]

displayLight : Bool -> String
displayLight state =
  if state then "Off" else "On!"
