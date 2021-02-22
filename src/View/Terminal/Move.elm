module View.Terminal.Move exposing (view, getMoveFromString, getString)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter)

import Model.Move
import Model.Attribute as Ma
import Repository.AttributeCollection
import Util.Predictive exposing (getWordWithDotCommand, showAttributes, getPredictive)

type Panel
    = Attribute
    | Amount
    | Before
    | After
    | Date
    | None

view : Repository.AttributeCollection.Model -> String -> Html msg
view acs strd =
    let
        str = split "." strd |> head |> withDefault ""
        last = split " " str |> List.reverse |> head |> withDefault ""
        move = split " " str |> tail |> withDefault [] |> join " " |> getMoveFromString acs
        panel = getInputPanelName str
        attributeName = strd |> split " " |> tail |> andThen head |> withDefault ""
        attributes = 
            case attributeName of
                "purpose" -> acs.purposeAttributeModel.attributes
                "place" -> acs.placeAttributeModel.attributes
                _ -> []
    in
    div [] 
        [ div [] [ Model.Move.htmlMsg move ]
        , case panel of
            Before -> getPredictive attributes last |> showAttributes
            After -> getPredictive attributes last |> showAttributes
            _ -> div [] []
        ]

getString : Repository.AttributeCollection.Model -> String -> String
getString acs strd =
    let
        str = split "." strd |> head |> withDefault ""
        panel = getInputPanelName str
        lastd = split " " strd |> List.reverse |> head |> withDefault ""
        attributeName = strd |> split " " |> tail |> andThen head |> withDefault ""
        attributes = 
            case attributeName of
                "purpose" -> acs.purposeAttributeModel.attributes
                "place" -> acs.placeAttributeModel.attributes
                _ -> []
        lastword =
            case panel of
                Before -> getWordWithDotCommand attributes lastd
                After -> getWordWithDotCommand attributes lastd
                _ -> lastd
    in
    split " " str |> List.reverse |> tail |> withDefault [] |> (::) lastword |> List.reverse |> String.join " "

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

getAttributeId : Ma.AttributeElements -> String -> Int
getAttributeId attributes str =
    filter (\n -> n.name == str) attributes
    |> List.map (\n -> n.id)
    |> head 
    |> withDefault 0

getInputPanelName : String -> Panel
getInputPanelName str =
    let
        len = split " " str |> List.length
    in
    case len of
        2 -> Attribute
        3 -> Amount
        4 -> Before
        5 -> After
        6 -> Date
        _ -> None


