module Model.Balance exposing (Balance, init, interpretation, encode, htmlMsg)

import Html exposing (..)
import Json.Encode as Encode exposing (..)

import Model.Attribute as Attribute

type alias Balance =
    { amount : Maybe Int
    , item : Maybe String
    , kind_id : Maybe Int
    , purpose_id : Maybe Int
    , place_id : Maybe Int
    , date : Maybe String
    }

interpretation : List Attribute.Attribute -> List Attribute.Attribute -> List Attribute.Attribute -> String -> Balance
interpretation kinds purposes places str =
    let
        stringList = String.split " " str |> Maybe.Just

        maybeHead = Maybe.andThen List.head
        maybeTail = Maybe.andThen List.tail
        maybeInt = Maybe.andThen String.toInt

        getKindNumber = getAttributeNumber kinds
        getPurposeNumber = getAttributeNumber purposes
        getPlaceNumber = getAttributeNumber places

        amount = stringList |> maybeHead |> maybeInt |> maybeMinus
        item = stringList |> maybeTail |> maybeHead
        kind_id = stringList |> maybeTail |> maybeTail |> maybeHead |> getKindNumber
        purpose_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> getPurposeNumber
        place_id = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead |> getPlaceNumber
        date = stringList |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeTail |> maybeHead
    in
        Balance amount item kind_id purpose_id place_id date

htmlMsg : Balance -> Html msg
htmlMsg balance =
    let
        amount = maybeIntText balance.amount 
        item = maybeStringText balance.item
        kind_id = maybeIntText balance.kind_id
        purpose_id = maybeIntText balance.purpose_id
        place_id = maybeIntText balance.place_id
        date = maybeStringText balance.date
    in  
        div[]
            [ text amount
            , br [] []
            , text item
            , br [] []
            , text kind_id
            , br [] []
            , text purpose_id
            , br [] []
            , text place_id
            , br [] []
            , text date
            , br [] []
            ]

init : Balance
init =
    Balance Nothing Nothing Nothing Nothing Nothing Nothing

encode : Balance -> Encode.Value
encode balance =
    Encode.object
        [ ("amount", Encode.int <| maybeIntNumber balance.amount)
        , ("item", Encode.string <| maybeStringText balance.item)
        , ("kind_id", Encode.int <| maybeIntNumber balance.kind_id)
        , ("purpose_id", Encode.int <| maybeIntNumber balance.purpose_id)
        , ("place_id", Encode.int <| maybeIntNumber balance.place_id)
        , ("date", Encode.string <| maybeStringText balance.date)
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





maybeIntNumber : Maybe Int -> Int
maybeIntNumber n =
    case n of
        Nothing ->
            0
        Just k ->
            k


maybeIntText : Maybe Int -> String
maybeIntText n =
    case n of
        Nothing ->
            ""
        Just k ->
            String.fromInt k

maybeStringText : Maybe String -> String
maybeStringText n =
    case n of
        Nothing ->
            ""
        Just k ->
            k

maybeMinus : Maybe Int -> Maybe Int
maybeMinus n =
    case n of
        Nothing -> Nothing
        Just k -> -1 * k |> Maybe.Just
        

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
