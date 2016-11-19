module Tests exposing (..)

import LightsOut

import Test exposing (..)
import Fuzz exposing (..)
import Expect

import Array exposing (get, empty, length)
import Maybe exposing (withDefault)

all : Test
all =
  describe "Creating the model"
    [ fuzz (intRange 4 10) "Grid should always be NxN" <|
      \n ->
        let
            model = LightsOut.model n
            xLen = model
                 |> .grid
                 |> length
            yLen = model
                 |> .grid
                 |> get 0
                 |> withDefault empty
                 |> length
        in
           Expect.true "Expected an NxN grid" <| List.all ((==) n) [xLen, yLen]
    ]
