port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Game exposing (..)
import IntPicker exposing (..)


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
    | Victory Int


type alias Model =
    { size : IntPicker.Model
    , game : GameState
    }


init : ( Model, Cmd Msg )
init =
    ( Model (IntPicker.init 2) Inactive, Cmd.none )



-- UPDATE


type Msg
    = SizeMsg IntPicker.Msg
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeMsg submsg ->
            handleSizeMsg submsg model

        GameMsg submsg ->
            handleGameMsg submsg model


handleSizeMsg : IntPicker.Msg -> Model -> ( Model, Cmd Msg )
handleSizeMsg msg model =
    case msg of
        Confirm size ->
            let
                ( submodel, subcmd ) =
                    Game.init size
            in
                ( { model | game = Active submodel }
                , Cmd.map GameMsg subcmd
                )

        _ ->
            ( { model | size = IntPicker.update msg model.size }
            , Cmd.none
            )


handleGameMsg : Game.Msg -> Model -> ( Model, Cmd Msg )
handleGameMsg msg model =
    case model.game of
        Active game ->
            let
                ( submodel, subcmd ) =
                    Game.update msg game
            in
                if Game.allOff submodel.grid then
                    ( { model | game = Victory submodel.moves }, Cmd.none )
                else
                    ( { model | game = Active submodel }, Cmd.map GameMsg subcmd )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        Inactive ->
            Html.map SizeMsg (IntPicker.view model.size)

        Active submodel ->
            Html.map GameMsg (Game.view submodel)

        Victory moves ->
            div [] [ text <| "Victory in " ++ (toString moves) ++ " moves!" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
