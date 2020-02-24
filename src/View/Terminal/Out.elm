module View.Terminal.Out exposing (getView, getSendAction)

import Html exposing (..)
import Html.Attributes
import Html.Events

import Model.Balance
import Model.Attribute as Ma

getSendAction : String -> Cmd msg
getSendAction s = Cmd.none

getView : Ma.Kinds -> Ma.Purposes -> Ma.Places -> String -> Html msg
getView kinds purposes places str =
    let
        balanceString = getBalanceString str
        balance = getBalanceFromString kinds purposes places balanceString
    in
    -- div [] [ text balanceString ]
    Model.Balance.htmlMsg balance

getBalanceFromString : Ma.Kinds -> Ma.Purposes -> Ma.Places -> String -> Model.Balance.Balance
getBalanceFromString kinds purposes places str =
    let
        strs = String.split " " str
        amount = maybeStringToInt <| List.head strs
        amountTail = maybeListStringToListString <| List.tail strs
        item = maybeStringToString <| List.head amountTail
        itemTail = maybeListStringToListString <| List.tail amountTail
        kind = getAttributeId kinds <| maybeStringToString <| List.head itemTail
        kindTail = maybeListStringToListString <| List.tail itemTail
        purpose = getAttributeId purposes <| maybeStringToString <| List.head kindTail
        purposeTail = maybeListStringToListString <| List.tail kindTail
        place = getAttributeId places <| maybeStringToString <| List.head purposeTail
        placeTail = maybeListStringToListString <| List.tail purposeTail
        date = maybeStringToString <| List.head placeTail
    in
    Model.Balance.Balance amount item kind purpose place date

maybeListStringToListString : Maybe (List String) -> List String
maybeListStringToListString strs =
    case strs of
        Nothing ->
            []
        Just strings ->
             strings

maybeStringToString : Maybe String -> String 
maybeStringToString s =
    case s of
        Nothing ->
            ""
        Just str ->
            str

maybeStringToInt : Maybe String -> Int
maybeStringToInt s =
    case s of
        Nothing ->
            0
        Just str ->
            stringToInt str

stringToInt : String -> Int
stringToInt s =
    let
        n = String.toInt s 
    in
    case n of
        Nothing ->
            0
        Just num ->
            num


getAttributeId : List Ma.Attribute -> String -> Int
getAttributeId attributes str =
    let
        filteredAttributes = List.filter (\n -> n.name == str) attributes
    in
        case (List.head filteredAttributes) of
            Nothing ->
                0
            Just a ->
                a.id

getBalanceString : String -> String
getBalanceString str =
    let
        balanceStr = List.tail <| String.split " " str
    in
    case balanceStr of
        Nothing ->
            ""
        Just s ->
            String.join " " s
    
