port module Grid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Grid.Model exposing (..)
import Grid.Update exposing (..)
import Grid.Message exposing (..)


type alias Msg = Grid.Message.Msg
type alias Model = Grid.Model.Model
type alias Grid = Grid.Model.Grid


-- VIEW


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
        , style
            [ ( "backgroundColor"
              , if state then
                    "red"
                else
                    "gray"
              )
            ]
        ]
        [ text "Click Me!" ]

