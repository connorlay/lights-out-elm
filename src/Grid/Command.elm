port module Grid.Command exposing (..)

import Grid.Message exposing (..)
import Random exposing (..)

randomBools: Int -> Cmd Msg
randomBools size =
    Random.generate NewGrid (Random.list (size ^ 2) Random.bool)
