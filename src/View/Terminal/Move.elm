module View.Terminal.Move exposing (view, getSendAction, Msg, init, update, Model)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Move
import Request.PostMove
import Model.Attribute as Ma
import Repository.AttributeCollection

type alias Model =
    { result : Request.PostMove.Model
    }

type Msg 
    = MovePost Request.PostMove.Msg

init : ( Model, Cmd Msg )
init = 
    let
        ( model, cmd ) = Request.PostMove.init
    in
    ( Model model, Cmd.map MovePost cmd )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MovePost msg_ ->
            let
                ( result, _ ) = Request.PostMove.update msg_ model.result
            in
            ( { model | result = result }, Cmd.none )

getSendAction : Repository.AttributeCollection.Model -> String -> Cmd Msg
getSendAction acs s = 
    let
        move = split " " s |> tail |> withDefault [] |> join " " |> getMoveFromString acs 
        cmd = Request.PostMove.post move 
    in
    Cmd.map MovePost cmd
-- getSendAction : String -> Cmd msg
-- getSendAction s = Cmd.none

view : Model -> Repository.AttributeCollection.Model -> String -> Html msg
view model acs str =
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

