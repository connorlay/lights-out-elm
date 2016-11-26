port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Game exposing (..)
import SizeSelector exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type GameState
    = Inactive
    | Active Game.Model


type alias Model =
    { size : SizeSelector.Model
    , game : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( Model (SizeSelector.init 3) Inactive, Cmd.none )



-- UPDATE


type Msg
    = SizeMsg SizeSelector.Msg
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeMsg submsg ->
            case submsg of
                Confirm size ->
                    let
                        ( submodel, subcmd ) =
                            Game.init size
                    in
                        ( { model | game = Active submodel }
                        , Cmd.map GameMsg subcmd
                        )

                _ ->
                    ( { model | size = SizeSelector.update submsg model.size }
                    , Cmd.none
                    )

        GameMsg submsg ->
            let
                ( submodel, _ ) =
                    updateGame submsg model.game
            in
                ( { model | game = submodel }, Cmd.none )


updateGame : Game.Msg -> GameState -> ( GameState, Cmd Game.Msg )
updateGame msg gameState =
    case gameState of
        Inactive ->
            ( gameState, Cmd.none )

        Active game ->
            let
                ( submodel, subcmd ) =
                    Game.update msg game
            in
                ( Active submodel, subcmd )



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        Inactive ->
            Html.map SizeMsg (SizeSelector.view model.size)

        Active submodel ->
            Html.map GameMsg (Game.view submodel)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
