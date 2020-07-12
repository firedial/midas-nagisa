module View.Terminal.Move exposing (view, getMoveFromString)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Move
import Model.Attribute as Ma
import Repository.AttributeCollection

view :  Repository.AttributeCollection.Model -> String -> Html msg
view acs str =
    let
        move = split " " str |> tail |> withDefault [] |> join " " |> getMoveFromString acs
    in
    -- div [] [ text balanceString ]
    div [] 
        [ div [] [ Model.Move.htmlMsg move ]
        ]

getMoveFromString : Repository.AttributeCollection.Model -> String -> Model.Move.Move
getMoveFromString acs str =
    let
        strs = split " " str

        attribute = head strs |> withDefault ""
        attributeTail = tail strs |> withDefault []

        attributes =
            case attribute of
                "purpose" -> 
                    acs.purposeAttributeModel.attributes
                "places" -> 
                    acs.placeAttributeModel.attributes
                _ ->
                    acs.placeAttributeModel.attributes

        amount = head attributeTail |> andThen toInt |> withDefault 0 
        amountTail = tail attributeTail |> withDefault []

        beforeId = head amountTail |> withDefault "" |> getAttributeId attributes
        beforeTail = tail amountTail |> withDefault []

        afterId = head beforeTail |> withDefault "" |> getAttributeId attributes
        afterTail = tail beforeTail |> withDefault []

        date = head afterTail |> withDefault ""
    in
    Model.Move.Move attribute amount beforeId afterId date

getAttributeId : List Ma.Attribute -> String -> Int
getAttributeId attributes str =
    filter (\n -> n.name == str) attributes
    |> map (\n -> n.id)
    |> head 
    |> withDefault 0

