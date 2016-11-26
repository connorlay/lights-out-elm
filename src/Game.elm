port module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Set exposing (..)
import Maybe exposing (..)
import Random exposing (..)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { grid : Grid }


type alias Grid =
    Array (Array Bool)


model : Int -> Model
model n =
    { grid = False |> Array.repeat n |> Array.repeat n }


init : ( Model, Cmd Msg )
init =
    ( model 4, Random.generate NewGrid (Random.list 16 Random.bool) )



-- UPDATE


type Msg
    = ToggleCell ( Int, Int )
    | NewGrid (List Bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGrid bools ->
            ( { model | grid = createGrid bools }, Cmd.none )

        ToggleCell coord ->
            ( { model | grid = updateGrid model.grid (neighbors coord) }, Cmd.none )


createGrid : List Bool -> Grid
createGrid bools =
    bools
        |> chunk (dimension bools)
        |> as2x2Array


dimension : List a -> Int
dimension list =
    list
        |> List.length
        |> toFloat
        |> sqrt
        |> ceiling


chunk : Int -> List a -> List (List a)
chunk k xs =
    if k == 0 then
        [ [] ]
    else if k < 0 then
        []
    else if List.length xs > k then
        List.take k xs :: chunk k (List.drop k xs)
    else
        [ xs ]


as2x2Array : List (List Bool) -> Array (Array Bool)
as2x2Array list =
    list
        |> List.map (Array.fromList)
        |> Array.fromList


updateGrid : Grid -> Set ( Int, Int ) -> Grid
updateGrid grid coords =
    Set.foldl toggleCell grid coords


neighbors : ( Int, Int ) -> Set ( Int, Int )
neighbors ( row, col ) =
    [ ( 0, 0 ), ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]
        |> Set.fromList
        |> Set.map (\( row_, col_ ) -> ( row + row_, col + col_ ))


toggleCell : ( Int, Int ) -> Grid -> Grid
toggleCell ( row, col ) grid =
    let
        toggledCell =
            grid
                |> get2x2 ( row, col )
                |> not

        cells =
            grid
                |> Array.get row
                |> withDefault Array.empty
                |> Array.set col toggledCell
    in
        Array.set row cells grid


get2x2 : ( Int, Int ) -> Grid -> Bool
get2x2 ( row, col ) grid =
    grid
        |> Array.get row
        |> withDefault Array.empty
        |> Array.get col
        |> withDefault False



-- VIEW


view : Model -> Html Msg
view model =
    model
        |> .grid
        |> gridAsHtml


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
