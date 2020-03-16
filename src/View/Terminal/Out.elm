module View.Terminal.Out exposing (view, getSendAction, Msg, init, update, Model)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Balance
import Model.Attribute as Ma
import Request.PostBalance 
import Repository.AttributeCollection

type alias Model =
    { result : Request.PostBalance.Model
    }

type Msg 
    = OutPost Request.PostBalance.Msg

init : ( Model, Cmd Msg )
init = 
    let
        ( model, cmd ) = Request.PostBalance.init
    in
    ( Model model, Cmd.map OutPost cmd )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OutPost msg_ ->
            let
                ( result, _ ) = Request.PostBalance.update msg_ model.result
            in
            ( { model | result = result }, Cmd.none )

getSendAction : Repository.AttributeCollection.Model -> String -> Cmd Msg
getSendAction acs s = 
    let
        balance = split " " s |> tail |> withDefault [] |> join " " |> getBalanceFromString acs 
        cmd = Request.PostBalance.post balance
    in
    Cmd.map OutPost cmd
-- getSendAction : String -> Cmd msg
-- getSendAction s = Cmd.none

view : Model -> Repository.AttributeCollection.Model -> String -> Html msg
view model acs str =
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
    Model.Balance.Balance amount item kind purpose place date

getAttributeId : List Ma.Attribute -> String -> Int
getAttributeId attributes str =
    filter (\n -> n.name == str) attributes
    |> map (\n -> n.id)
    |> head 
    |> withDefault 0

