module Model.Balance exposing (Balance, Balances, init, htmlMsg, encodeBalance)

import Html exposing (..)
import Json.Encode as Encode exposing (..)

import Model.Attribute as Attribute

type alias Balance =
    { balanceId : Int
    , amount : Int
    , item : String
    , kindId : Int
    , purposeId : Int
    , placeId : Int
    , date : String
    }

type alias Balances = List Balance

htmlMsg : Balance -> Html msg
htmlMsg balance =
    div[]
        [ text <| (++) "amount: " <| String.fromInt balance.amount
        , br [] []
        , text <| (++) "item: " balance.item
        , br [] []
        , text <| (++) "kind_id: " <| String.fromInt balance.kindId
        , br [] []
        , text <| (++) "purpose_id: " <| String.fromInt balance.purposeId
        , br [] []
        , text <| (++) "place_id: " <| String.fromInt balance.placeId
        , br [] []
        , text <| (++) "date: " balance.date
        , br [] []
        ]

init : Balance
init = Balance 0 0 "" 0 0 0 ""


-- decodeBalance : Decode.Decoder Balance
-- decodeBalance =
--     Decode.map6 Balance
--         (field "amount" Decode.int)
--         (field "item" Decode.string)
--         (field "kind_id" Decode.int)
--         (field "purpose_id" Decode.int)
--         (field "place_id" Decode.int)
--         (field "date" Decode.string)


encodeBalance : Balance -> Encode.Value
encodeBalance balance =
    Encode.object
        [ ("amount", Encode.int balance.amount)
        , ("item", Encode.string balance.item)
        , ("kind_id", Encode.int balance.kindId)
        , ("purpose_id", Encode.int balance.purposeId)
        , ("place_id", Encode.int balance.placeId)
        , ("date", Encode.string balance.date)
        ]
