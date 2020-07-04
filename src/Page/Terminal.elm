module Page.Terminal exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import String

import Model.Balance
import Model.AttributeCollection
import Config.Env

import View.Terminal.Out
import View.Terminal.Move
import Repository.AttributeCollection

type alias Model =
    { command : Command
    , acsModel : Repository.AttributeCollection.Model
    , input : String
    }

type Command
    = None
    | Out View.Terminal.Out.Model
    | Move View.Terminal.Move.Model

init : ( Model, Cmd Msg )
init = 
    let
        ( acsModel, cmd ) = Repository.AttributeCollection.init
    in
    ( Model None acsModel "" , Cmd.none )

type Msg
    = Send
    | Input String
    | GetAttributeCollection Repository.AttributeCollection.Msg
    | OutMsg View.Terminal.Out.Msg
    | MoveMsg View.Terminal.Move.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            let
                ( outModel, _ ) = View.Terminal.Out.init
            in
            ( { model | input = str, command = Out outModel }, Cmd.none )
        Send ->
            ( model, Cmd.none )
        OutMsg msg_ ->
            case model.command of
                Out outModel ->
                    let
                        ( newModel, newCmd ) = View.Terminal.Out.update msg_ outModel
                    in
                    ( { model | command = Out newModel }, Cmd.map OutMsg newCmd)
                _ ->
                    ( model, Cmd.none )
        MoveMsg msg_ ->
            case model.command of
                Move moveModel ->
                    let
                        ( newModel, newCmd ) = View.Terminal.Move.update msg_ moveModel
                    in
                    ( { model | command = Out newModel }, Cmd.map MoveMsg newCmd)
                _ ->
                    ( model, Cmd.none )
        GetAttributeCollection msg_ ->
            let
                ( attributeCollectionModel, _ ) = Repository.AttributeCollection.update msg_ model.acsModel
            in
            ( { model | acsModel = attributeCollectionModel }, Cmd.none)

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
        , br [] []
        , case model.command of
            None ->
                Html.text "none"
            Out outModel ->
                View.Terminal.Out.view outModel model.acsModel model.input |> Html.map OutMsg
            Move moveModel ->
                View.Terminal.Move.view moveModel model.acsModel model.input |> Html.map MoveMsg
        ]

