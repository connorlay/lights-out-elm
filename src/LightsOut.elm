port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Game exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type GameState
    = Inactive Game.Model
    | Active Game.Model


type alias Model =
    { size : Int
    , game : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( Model 3 (Inactive (Game.model 0)), Cmd.none )



-- UPDATE


type Msg
    = IncrementSize
    | DecrementSize
    | StartGame
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementSize ->
            ( { model | size = model.size + 1 }, Cmd.none )

        DecrementSize ->
            ( { model | size = model.size - 1 }, Cmd.none )

        StartGame ->
            let
                ( gamemodel, subcmd ) =
                    Game.init model.size

                submodel =
                    Active gamemodel
            in
                ( { model | game = submodel }, Cmd.map GameMsg subcmd )

        GameMsg gamemsg ->
            let
                game =
                    case model.game of
                        Inactive game ->
                            game

                        Active game ->
                            game

                ( submodel, subcmd ) =
                    Game.update gamemsg game
            in
                ( { model | game = Active submodel }, Cmd.map GameMsg subcmd )



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        Inactive submodel ->
            div []
                [ text <| toString model.size
                , button [ onClick IncrementSize ] [ text "+" ]
                , button [ onClick DecrementSize ] [ text "-" ]
                , button [ onClick StartGame ] [ text "Go!" ]
                ]

        Active submodel ->
            Html.map GameMsg (Game.view submodel)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
