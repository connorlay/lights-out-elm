port module SizeSelector exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { size : Int }


init : Int -> Model
init n =
    Model n



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Confirm Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | size = model.size + 1 }

        Decrement ->
            { model | size = model.size - 1 }

        Confirm _ ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text <| toString model.size
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick (Confirm model.size) ] [ text "Go!" ]
        ]
