port module LightsOut exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Grid.Update exposing (..)
import Grid.View exposing (..)
import IntPicker exposing (..)
import Css
    exposing
        ( property
        , textAlign
        , center
        , color
        )
import Colors exposing (..)


type alias GridModel =
    Grid.Model.Model


type alias GridMsg =
    Grid.Message.Msg


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type GridState
    = Inactive
    | Active GridModel
    | Victory Int


type alias Model =
    { size : IntPicker.Model
    , game : GridState
    }


init : ( Model, Cmd Msg )
init =
    ( Model (IntPicker.init 2) Inactive, Cmd.none )



-- UPDATE


type Msg
    = SizeMsg IntPicker.Msg
    | GridMsg GridMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SizeMsg submsg ->
            handleSizeMsg submsg model

        GridMsg submsg ->
            handleGridMsg submsg model


handleSizeMsg : IntPicker.Msg -> Model -> ( Model, Cmd Msg )
handleSizeMsg msg model =
    case msg of
        Confirm size ->
            let
                ( submodel, subcmd ) =
                    Grid.Model.init size
            in
                ( { model | game = Active submodel }
                , Cmd.map GridMsg subcmd
                )

        _ ->
            ( { model | size = IntPicker.update msg model.size }
            , Cmd.none
            )


handleGridMsg : GridMsg -> Model -> ( Model, Cmd Msg )
handleGridMsg msg model =
    case model.game of
        Active game ->
            let
                ( submodel, subcmd ) =
                    Grid.Update.update msg game
            in
                if Grid.Update.allOff submodel.grid then
                    ( { model | game = Victory submodel.moves }, Cmd.none )
                else
                    ( { model | game = Active submodel }, Cmd.map GridMsg subcmd )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.game of
        Inactive ->
            div []
                [ div
                    [ styles
                        [ Css.property "font-size" "8em"
                        , textAlign center
                        , Css.color darkBlue
                        ]
                    ]
                    [ text "Lights Out!" ]
                , Html.map SizeMsg (IntPicker.view model.size)
                ]

        Active submodel ->
            Html.map GridMsg (Grid.View.view submodel)

        Victory moves ->
            div [] [ text <| "Victory in " ++ (toString moves) ++ " moves!" ]


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style
