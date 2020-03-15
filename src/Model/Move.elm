module Model.Move exposing (Move, htmlMsg)

import Html exposing (..)
import Json.Encode as Encode exposing (..)

import Model.Attribute as Attribute

type alias Move =
    { attributeName : String
    , amount : Int
    , beforeId : Int
    , afterId : Int
    , date : String
    }

htmlMsg : Move -> Html msg
htmlMsg move =
    div[]
        [ text <| (++) "attributeName: " move.attributeName
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


