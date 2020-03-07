module Request.Balance exposing (post, Msg)

import Http exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Balance

type Msg 
    = Receive (Result Http.Error String)

post : Model.Balance.Balance -> Cmd Msg
post balance =
    Http.request
        { method = "POST"
        , headers = []
        , url = getPostUrl
        , body = encode balance |> Http.jsonBody
        , expect = Http.expectJson Receive Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }


encode : Model.Balance.Balance -> Encode.Value
encode balance =
    Encode.object
        [ ("amount", Encode.int balance.amount)
        , ("item", Encode.string balance.item)
        , ("kind_id", Encode.int balance.kindId)
        , ("purpose_id", Encode.int balance.purposeId)
        , ("place_id", Encode.int balance.placeId)
        , ("date", Encode.string balance.date)
        ]


getPostUrl : String
getPostUrl = "http://localhost:3333/misuzu/api/v1/balance/"
