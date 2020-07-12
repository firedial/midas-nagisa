module View.Terminal.Out exposing (view, getBalanceFromString)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Balance
import Model.Attribute as Ma
import Repository.AttributeCollection

view : Repository.AttributeCollection.Model -> String -> Html msg
view acs str =
    let
        balance = split " " str |> tail |> withDefault [] |> join " " |> getBalanceFromString acs
    in
    -- div [] [ text balanceString ]
    div [] 
        [ div [] [ Model.Balance.htmlMsg balance ]
        ]

getBalanceFromString : Repository.AttributeCollection.Model -> String -> Model.Balance.Balance
getBalanceFromString acs str =
    let
        kinds = acs.kindAttributeModel.attributes
        purposes = acs.purposeAttributeModel.attributes
        places = acs.placeAttributeModel.attributes
        strs = split " " str

        amount = head strs |> andThen toInt |> withDefault 0 |> (*) -1
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
    Model.Balance.Balance 0 amount item kind purpose place date

getAttributeId : List Ma.Attribute -> String -> Int
getAttributeId attributes str =
    filter (\n -> n.name == str) attributes
    |> map (\n -> n.id)
    |> head 
    |> withDefault 0

