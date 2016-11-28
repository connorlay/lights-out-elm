port module Grid.Model exposing (..)

import Grid.Message exposing (..)
import Grid.Command exposing (..)
import Array exposing (..)

type alias Msg = Grid.Message.Msg

type alias Model =
    { grid : Grid
    , moves : Int
    }


type alias Grid =
    Array (Array Bool)


model : Int -> Model
model n =
    { grid = False |> Array.repeat n |> Array.repeat n
    , moves = 0
    }


init : Int -> ( Model, Cmd Msg )
init size =
    ( model size, Grid.Command.randomBools size )




