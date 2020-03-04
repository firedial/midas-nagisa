module Request.Post exposing (post, Msg)

import Http exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

type Msg 
    = Receive (Result Http.Error String)

post : String -> Encode.Value -> Cmd Msg
post url encode =
    let
        body = encode |> Http.jsonBody
    in
        Http.request
            { method = "POST"
            , headers = []
            , url = url
            , body = body
            , expect = Http.expectJson Receive Decode.string
            , timeout = Nothing
            , tracker = Nothing
            }

