module Tests exposing (..)

import LightsOut exposing (..)

import Test exposing (..)
import Fuzz exposing (..)
import Expect

import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)


all : Test
all =
  describe "LightsOut" [

    describe "Creating the model"
      [ fuzz (intRange 4 10) "Grid should always be NxN" <|
        \n ->
          let
              model = LightsOut.model n
              xLen = model
                   |> .grid
                   |> Array.length
              yLen = model
                   |> .grid
                   |> get 0
                   |> withDefault Array.empty
                   |> Array.length
          in
             Expect.true "Expected an NxN grid" <| List.all ((==) n) [xLen, yLen]
      ],

    describe "Finding cells to toggle"
      [ test "A set of coords to toggle should be returned" <|
        \() ->
          let
             expected = Set.fromList [ (1, 1), (0, 1), (1, 0), (2, 1), (1, 2) ]
          in
             Expect.equal expected <| LightsOut.toToggle (1, 1)
      ],

    describe "Toggling cells in a grid"
      [ test "Cells should toggle based on coords in set" <|
        \() ->
          let
            coords = Set.fromList [ (1, 1), (0, 1), (1, 0), (2, 1), (1, 2) ]
            grid = 4
                 |> LightsOut.model
                 |> .grid
            expected = [
                   [ False, True,  False, False ],
                   [ True,  True,  True,  False ],
                   [ False, True,  False, False ],
                   [ False, False, False, False ] ]
                   |> List.map (Array.fromList)
                   |> Array.fromList
          in
            Expect.equal expected <| LightsOut.updateGrid grid coords
       ]
     ]
