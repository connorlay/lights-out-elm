port module SizeSelector exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { size : Int
    , confirmed : Bool
    }


init : Int -> ( Model, Cmd Msg )
init n =
    ( Model n False, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Confirm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | size = model.size + 1 }, Cmd.none )

        Decrement ->
            ( { model | size = model.size - 1 }, Cmd.none )

        Confirm ->
            ( { model | confirmed = True }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text <| toString model.size
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Confirm ] [ text "Go!" ]
        ]
