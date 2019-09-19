module Model.Balance exposing (Balance, init, interpretation, encode, htmlMsg)

import Html exposing (..)
import Json.Encode as Encode exposing (..)

import Model.Attribute as Attribute

type alias Balance =
    { amount : Int
    , item : String
    , kindId : Int
    , purposeId : Int
    , placeId : Int
    , date : String
    }

interpretation : List Attribute.Attribute -> List Attribute.Attribute -> List Attribute.Attribute -> String -> Balance
interpretation kinds purposes places str = init
-- interpretation kinds purposes places str =
--     let
--         stringList = String.split " " str |> Maybe.Just
-- 
--         maybeHead = Maybe.andThen List.head
--         maybeTail = Maybe.andThen List.tail
--         maybeInt = Maybe.andThen String.toInt
-- 
--         getKindNumber = getAttributeNumber kinds
--         getPurposeNumber = getAttributeNumber purposes
--         getPlaceNumber = getAttributeNumber places
-- 
--         amount = stringList |> maybeHead |> maybeInt |> maybeMinus
--         item = stringList |> maybeTail |> maybeHead
--         kind_id = stringList |> maybeTail |> maybeTail |> maybeHead |> getKindNumber
--         purpose_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> getPurposeNumber
--         place_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> getPlaceNumber
--         date = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead
--     in
--         Balance amount item kind_id purpose_id place_id date

htmlMsg : Balance -> Html msg
htmlMsg balance =
    div[]
        [ text <| String.fromInt balance.amount
        , br [] []
        , text balance.item
        , br [] []
        , text <| String.fromInt balance.kindId
        , br [] []
        , text <| String.fromInt balance.purposeId
        , br [] []
        , text <| String.fromInt balance.placeId
        , br [] []
        , text balance.date
        , br [] []
        ]

init : Balance
init = Balance 0 "" 0 0 0 ""

encode : Balance -> Encode.Value
encode balance =
    Encode.object
        [ ("amount", Encode.int balance.amount)
        , ("item", Encode.string balance.item)
        , ("kind_id", Encode.int balance.kindId)
        , ("purpose_id", Encode.int balance.purposeId)
        , ("place_id", Encode.int balance.placeId)
        , ("date", Encode.string balance.date)
        ]

-- decodeBalance : Decode.Decoder Balance
-- decodeBalance =
--     Decode.map6 Balance
--         (field "amount" Decode.int)
--         (field "item" Decode.string)
--         (field "kind_id" Decode.int)
--         (field "purpose_id" Decode.int)
--         (field "place_id" Decode.int)
--         (field "date" Decode.string)


getAttributeNumber : List Attribute.Attribute -> Maybe String -> Maybe Int
getAttributeNumber attributes str =
    case str of
        Nothing ->
            Nothing
        Just s ->
            let
                attribute = List.filter (\n -> n.name == s) attributes
            in
                case (List.head attribute) of
                    Just f ->
                        Maybe.Just f.id
                    Nothing ->
                        Nothing
