port module Grid.Message exposing (..)

type Msg
    = NewGrid (List Bool)
    | ToggleCell ( Int, Int )
