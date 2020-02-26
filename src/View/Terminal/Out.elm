module View.Terminal.Out exposing (getView, getSendAction)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Balance
import Model.Attribute as Ma

getSendAction : String -> Cmd msg
getSendAction s = Cmd.none

getView : Ma.Kinds -> Ma.Purposes -> Ma.Places -> String -> Html msg
getView kinds purposes places str =
    let
        balance = split " " str |> tail |> withDefault [] |> join " " |> getBalanceFromString kinds purposes places
    in
    -- div [] [ text balanceString ]
    Model.Balance.htmlMsg balance

getBalanceFromString : Ma.Kinds -> Ma.Purposes -> Ma.Places -> String -> Model.Balance.Balance
getBalanceFromString kinds purposes places str =
    let
        strs = split " " str

        amount = head strs |> andThen toInt |> withDefault 0
        amountTail = tail strs |> withDefault []

        item = head amountTail |> withDefault ""
        itemTail = tail amountTail |> withDefault []

        kind = head itemTail |> withDefault "" |> getAttributeId kinds
        kindTail = tail itemTail |> withDefault []

        purpose = head kindTail |> withDefault "" |> getAttributeId purposes 
        purposeTail = tail kindTail |> withDefault []

        place = head purposeTail |> withDefault "" |> getAttributeId places
        placeTail = tail purposeTail |> withDefault []

        date = head placeTail |> withDefault ""
    in
    Model.Balance.Balance amount item kind purpose place date

getAttributeId : List Ma.Attribute -> String -> Int
getAttributeId attributes str =
    filter (\n -> n.name == str) attributes
    |> map (\n -> n.id)
    |> head 
    |> withDefault 0

