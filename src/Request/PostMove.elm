module Request.PostMove exposing (post, Msg, Model, init, update)

import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

import Model.Move
import Request.Util

type alias Model =
    { msg : String
    }

type Msg 
    = Receive (Result Http.Error String)

init : ( Model, Cmd Msg )
init = ( Model "", Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Receive result ->
            case result of
                Ok msg_ ->
                    ( { model | msg = msg_ }, Cmd.none )
                Err err ->
                    ( { model | msg = Request.Util.getErrMsg err }, Cmd.none )

post : Model.Move.Move -> Cmd Msg
post move =
    Http.request
        { method = "POST"
        , headers = []
        , url = getPostUrl
        , body = encode move |> Http.jsonBody
        , expect = Http.expectJson Receive Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }

encode : Model.Move.Move -> Encode.Value
encode move =
    Encode.object
        [ ("attribute", Encode.string move.attribute)
        , ("amount", Encode.int move.amount)
        , ("before_id", Encode.int move.beforeId)
        , ("after_id", Encode.int move.afterId)
        , ("date", Encode.string move.date)
        ]


getPostUrl : String
getPostUrl = "http://localhost:3333/misuzu/api/v1/move/"
