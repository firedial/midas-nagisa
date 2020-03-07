module Request.Balance exposing (get, post, Msg)

import Http exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Balance

type Msg 
    = Receive (Result Http.Error String)
    | GetBalances (Result Http.Error Model.Balance.Balance)

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

get : Cmd Msg
get =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = getGetUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GetBalances decodeBalance
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

decodeBalance : Decode.Decoder Model.Balance.Balance
decodeBalance =
    Decode.map6 Model.Balance.Balance
        (field "amount" Decode.int)
        (field "item" Decode.string)
        (field "kind_id" Decode.int)
        (field "purpose_id" Decode.int)
        (field "place_id" Decode.int)
        (field "date" Decode.string)

getPostUrl : String
getPostUrl = "http://localhost:3333/misuzu/api/v1/balance/"

getGetUrl : String
getGetUrl = "http://localhost:3333/misuzu/api/v1/balance/"
