port module Grid.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (backgroundColor, asPairs, rgb)
import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Array exposing (..)


type alias Msg =
    Grid.Message.Msg


type alias Model =
    Grid.Model.Model


type alias Grid =
    Grid.Model.Grid


view : Model -> Html Msg
view model =
    div []
        [ model |> .grid |> gridAsHtml
        , text <| "Moves: " ++ (toString model.moves)
        ]


gridAsHtml : Grid -> Html Msg
gridAsHtml grid =
    grid
        |> toIndexedList
        |> List.foldr (\row acc -> (rowAsHtml row) :: acc) []
        |> div []


rowAsHtml : ( Int, Array Bool ) -> Html Msg
rowAsHtml ( row, cells ) =
    cells
        |> toIndexedList
        |> List.foldr
            (\( col, state ) acc -> cellAsHtml ( row, col ) state :: acc)
            []
        |> div []


cellAsHtml : ( Int, Int ) -> Bool -> Html Msg
cellAsHtml coord state =
    button
        [ onClick (ToggleCell coord)
        , styles [ backgroundColor (if state then rgb 255 255 56 else rgb 124 124 124)]
        ]
        [ text "Click Me!" ]

styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style
