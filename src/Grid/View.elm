port module Grid.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (margin, solid, border, borderRadius, vh, width, px, height, backgroundColor, asPairs, rgb, displayFlex, alignItems, center, property, maxWidth, pct)
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
    div
        [ styles
            [ displayFlex
            , alignItems center
            , Css.property "justify-content" "center"
            , Css.height (vh 100)
            ]
        ]
        [ model |> .grid |> gridAsHtml ]


gridAsHtml : Grid -> Html Msg
gridAsHtml grid =
    grid
        |> toIndexedList
        |> List.foldr (\row acc -> (rowAsHtml row) :: acc) []
        |> div
            [ styles
                [ maxWidth (pct 50) ]
            ]


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
        , styles
            [ backgroundColor
                (if state then
                    rgb 255 255 56
                 else
                    rgb 124 124 124
                )
            , Css.width (px 100)
            , Css.height (px 100)
            , border (px 0)
            , borderRadius (px 15)
            , margin (px 2)
            ]
        ]
        []


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style
