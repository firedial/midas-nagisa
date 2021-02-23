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
import Model.Move
import Model.Attribute
import Config.Env

import Page.Terminal.Out
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
    | Out Page.Terminal.Out.Model

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
    | Receive (Result Http.Error String)
    | OutMsg Page.Terminal.Out.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            let
                command = str |> String.split " " |> List.head |> Maybe.withDefault ""
            in
            case command of
                "out" ->
                    let
                        ( outModel, _ ) = Page.Terminal.Out.init str
                    in
                    ( { model | command = Out outModel, input = str }, Cmd.none )
                _ ->
                    ( { model | input = str }, Cmd.none )
        Send ->
            ( model, Cmd.none )
        GetAttributeCollection msg_ ->
            let
                ( attributeCollectionModel, _ ) = Repository.AttributeCollection.update msg_ model.acsModel
            in
            ( { model | acsModel = attributeCollectionModel }, Cmd.none)
        Receive result ->
            ( model, Cmd.none )
        OutMsg msg_ ->
            case model.command of
                Out model_ ->
                    let 
                        ( newModel, newCmd ) =
                            Page.Terminal.Out.update msg_ model_
                    in
                    ( { model | command = Out newModel }
                    , Cmd.map OutMsg newCmd
                    )
                _ ->
                    ( model, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , div []
            [
                case model.command of
                    None ->
                        div []
                            [ Html.form 
                                [ Html.Events.onSubmit Send ]
                                [ input 
                                    [ Html.Attributes.value <| model.input, Html.Events.onInput Input, Html.Attributes.autofocus True ]
                                    []
                                , button
                                    []
                                    [ text "Submit" ]
                                ]
                            , Html.text "none"
                            ]
                    Out model_ ->
                        Page.Terminal.Out.view model_ |> Html.map OutMsg
            ]
        ]

postMove : Model.Move.Move -> Cmd Msg
postMove move =
    Http.request
        { method = "POST"
        , headers = []
        , url = Config.Env.getApiUrl ++ "/move/"
        , body = encodeMove move |> Http.jsonBody
        , expect = Http.expectJson Receive Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }

encodeMove : Model.Move.Move -> Encode.Value
encodeMove move =
    Encode.object
        [ ("attribute", Encode.string move.attribute)
        , ("amount", Encode.int move.amount)
        , ("before_id", Encode.int move.beforeId)
        , ("after_id", Encode.int move.afterId)
        , ("date", Encode.string move.date)
        ]
