module Page.Detail exposing (Model, Msg, init, update, view)

import Html exposing (..)

type alias Model =
    { name : String
    , count : Int
    }

init : String -> Int -> ( Model, Cmd Msg )
init name count =
    ( Model name count
    , Cmd.none
    )

type Msg
    = Ok
    | Ng

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Ok ->
            ( { model | name = "ok" }, Cmd.none)
        Ng ->
            ( { model | name = "ng" }, Cmd.none)

view : Model -> Html Msg
view model =
    text model.name
