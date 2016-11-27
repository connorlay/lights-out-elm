module Tests exposing (..)

import Game exposing (..)
import Test exposing (..)
import Fuzz exposing (..)
import Expect
import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)


all : Test
all =
    describe "Game"
        [ describe "Creating the model"
            [ fuzz (intRange 4 10) "Grid should always be NxN" <|
                \n ->
                    let
                        model =
                            Game.model n

                        xLen =
                            model
                                |> .grid
                                |> Array.length

                        yLen =
                            model
                                |> .grid
                                |> get 0
                                |> withDefault Array.empty
                                |> Array.length
                    in
                        Expect.true "Expected an NxN grid" <| List.all ((==) n) [ xLen, yLen ]
            ]
        , describe "Creating the grid from a list of bools"
            [ test "A 3x3 grid should be created from a list of 16 bools" <|
                \() ->
                    let
                        bools =
                            [ True, False, False, True, True, False, True, True, True ]

                        expected =
                            Game.as2x2Array
                                [ [ True, False, False ]
                                , [ True, True, False ]
                                , [ True, True, True ]
                                ]
                    in
                        Expect.equal expected <| Game.createGrid bools
            ]
        , describe "Finding cells to toggle"
            [ test "A set of coords to toggle should be returned" <|
                \() ->
                    let
                        expected =
                            Set.fromList [ ( 1, 1 ), ( 0, 1 ), ( 1, 0 ), ( 2, 1 ), ( 1, 2 ) ]
                    in
                        Expect.equal expected <| Game.neighbors ( 1, 1 )
            ]
        , describe "Toggling cells in a grid"
            [ test "Cells should toggle based on coords in set" <|
                \() ->
                    let
                        coords =
                            Set.fromList [ ( 1, 1 ), ( 0, 1 ), ( 1, 0 ), ( 2, 1 ), ( 1, 2 ) ]

                        grid =
                            4
                                |> Game.model
                                |> .grid

                        expected =
                            Game.as2x2Array
                                [ [ False, True, False, False ]
                                , [ True, True, True, False ]
                                , [ False, True, False, False ]
                                , [ False, False, False, False ]
                                ]
                    in
                        Expect.equal expected <| Game.updateGrid grid coords
            , test "Cells out of bounds should not toggle" <|
                \() ->
                    let
                        coords =
                            Set.fromList [ ( 3, 3 ), ( 2, 3 ), ( 3, 2 ), ( 3, 4 ), ( 4, 3 ) ]

                        grid =
                            4
                                |> Game.model
                                |> .grid

                        expected =
                            Game.as2x2Array
                                [ [ False, False, False, False ]
                                , [ False, False, False, False ]
                                , [ False, False, False, True ]
                                , [ False, False, True, True ]
                                ]
                    in
                        Expect.equal expected <| Game.updateGrid grid coords
            ]
        , describe "Victory detection" <|
            [ test "Grid should be a victory if all cells are off" <|
                \() ->
                    let
                        grid =
                            4
                                |> Game.model
                                |> .grid
                    in
                        Expect.true "Grid is a victory" <| Game.victory grid
            , test "Grid should not be a victory if any cells are on" <|
                \() ->
                    let
                        grid =
                            4
                                |> Game.model
                                |> .grid
                                |> Game.toggleCell ( 0, 0 )
                    in
                        Expect.false "Grid is not a victory" <| Game.victory grid
            ]
        ]
