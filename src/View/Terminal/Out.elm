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

type Panel
    = Amount
    | Item
    | Kind
    | Purpose
    | Place
    | Date
    | None

view : Repository.AttributeCollection.Model -> String -> Html msg
view acs str =
    let
        balance = split " " str |> tail |> withDefault [] |> join " " |> getBalanceFromString acs
        panel = getInputPanelName str
        last = split " " str |> List.reverse |> head |> withDefault ""
    in
    -- div [] [ text balanceString ]
    div [] 
        [ div [] [ Model.Balance.htmlMsg balance ]
        , case panel of
            Kind -> getPredictive acs.kindAttributeModel.attributes last |> showAttributes
            Purpose -> getPredictive acs.purposeAttributeModel.attributes last |> showAttributes
            Place -> getPredictive acs.placeAttributeModel.attributes last |> showAttributes
            _ -> div [] []
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

showAttributes : List Ma.Attribute -> Html msg
showAttributes attributes =
    let
        predictiveList = List.take 8 attributes
    in
    div [] ( map (\a -> div [] [ text a.name ]) predictiveList )

getPredictive : List Ma.Attribute -> String -> List Ma.Attribute
getPredictive attributes str =
    filter (\n -> String.startsWith str n.name) attributes

getInputPanelName : String -> Panel
getInputPanelName str =
    let
        len = split " " str |> List.length
    in
    case len of
        2 -> Amount
        3 -> Item
        4 -> Kind
        5 -> Purpose
        6 -> Place
        7 -> Date
        _ -> None

