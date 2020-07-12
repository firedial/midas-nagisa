module Page.Terminal exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)


import Model.Balance
import Model.AttributeCollection
import Config.Env

import Request.PostBalance 
import View.Terminal.Out
import View.Terminal.Move
import Repository.AttributeCollection
import Request.Util

type alias Model =
    { command : Command
    , acsModel : Repository.AttributeCollection.Model
    , input : String
    , error : String
    }

type Command
    = None
    | Out
    | Move

init : ( Model, Cmd Msg )
init = 
    let
        ( acsModel, cmd ) = Repository.AttributeCollection.init
    in
    ( Model None acsModel "" "" , Cmd.map GetAttributeCollection cmd )

type Msg
    = Send
    | Input String
    | GetAttributeCollection Repository.AttributeCollection.Msg
    | Recieve (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            let
                cmd = getCommand str
            in
            ( { model | input = str, command = cmd }, Cmd.none )
        Send ->
            case model.command of
                Out ->
                    let
                        balance = split " " model.input |> tail |> withDefault [] |> join " " |> View.Terminal.Out.getBalanceFromString model.acsModel
                        cmd = postBalance balance
                    in
                    ( model, cmd )
                _ -> ( model, Cmd.none )
        GetAttributeCollection msg_ ->
            let
                ( attributeCollectionModel, _ ) = Repository.AttributeCollection.update msg_ model.acsModel
            in
            ( { model | acsModel = attributeCollectionModel }, Cmd.none)
        Recieve result ->
            case result of
                Ok msg_ ->
                    ( { model | input = "", error = msg_ }, Cmd.none )
                Err err ->
                    ( { model | error = Request.Util.getErrMsg err }, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , div []
            [ Html.form 
                [ Html.Events.onSubmit Send ]
                [ input 
                    [ Html.Attributes.value <| model.input, Html.Events.onInput Input ]
                    []
                , button
                    []
                    [ text "Submit" ]
                ]
            ] 
        , br [] [ ]
        , Html.text model.error
        , case model.command of
            None ->
                Html.text "none"
            Out ->
                View.Terminal.Out.view model.acsModel model.input
            Move ->
                View.Terminal.Out.view model.acsModel model.input
        ]

getCommand : String -> Command
getCommand str =
    let
        cmd = String.split " " str |> List.head
    in
    case cmd of
        Just cmd_ ->
            case cmd_ of
                "out" -> Out
                "move" -> Move
                _ -> None
        Nothing -> None

postBalance : Model.Balance.Balance -> Cmd Msg
postBalance balance =
    Http.request
        { method = "POST"
        , headers = []
        , url = Config.Env.getApiUrl ++ "/balance/"
        , body = encodeBalance balance |> Http.jsonBody
        , expect = Http.expectJson Recieve Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }

encodeBalance : Model.Balance.Balance -> Encode.Value
encodeBalance balance =
    Encode.object
        [ ("amount", Encode.int balance.amount)
        , ("item", Encode.string balance.item)
        , ("kind_id", Encode.int balance.kindId)
        , ("purpose_id", Encode.int balance.purposeId)
        , ("place_id", Encode.int balance.placeId)
        , ("date", Encode.string balance.date)
        ]

 