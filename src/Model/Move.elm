module Model.Move exposing (Move, htmlMsg, encodeMove)

import Html exposing (..)
import Json.Encode

import Model.Attribute as Attribute

type alias Move =
    { attribute : String
    , amount : Int
    , beforeId : Int
    , afterId : Int
    , date : String
    }

htmlMsg : Move -> Html msg
htmlMsg move =
    div[]
        [ text <| (++) "attribute: " move.attribute
        , br [] []
        , text <| (++) "amount: " <| String.fromInt move.amount
        , br [] []
        , text <| (++) "beforeId: " <| String.fromInt move.beforeId
        , br [] []
        , text <| (++) "afterId: " <| String.fromInt move.afterId
        , br [] []
        , text <| (++) "date: " move.date
        , br [] []
        ]

encodeMove : Move -> Json.Encode.Value
encodeMove move =
    Json.Encode.object
        [ ("attribute", Json.Encode.string move.attribute)
        , ("amount", Json.Encode.int move.amount)
        , ("before_id", Json.Encode.int move.beforeId)
        , ("after_id", Json.Encode.int move.afterId)
        , ("date", Json.Encode.string move.date)
        ]


