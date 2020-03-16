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
    { input : String
    , error : String
    , balance : Model.Balance.Balance
    , acsModel : Repository.AttributeCollection.Model
    , outModel : View.Terminal.Out.Model
    , moveModel : View.Terminal.Move.Model
    }

init : ( Model, Cmd Msg )
init = 
    let
        ( outModel, _ ) = View.Terminal.Out.init
        ( moveModel, _ ) = View.Terminal.Move.init
        ( acsModel, cmd ) = Repository.AttributeCollection.init
    in
    ( Model "" "" Model.Balance.init acsModel outModel moveModel, Cmd.map GetAttributeCollection cmd )

type Command = None | Out | Move

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
            ( { model | input = str }, Cmd.none )
        Send ->
            ( { model | error = "send" }, getCommandSend model)
        OutMsg msg_ ->
            let
                ( outModel, _ ) = View.Terminal.Out.update msg_ model.outModel
                input = if outModel.result.msg == "OK" then "" else model.input
                error = outModel.result.msg
            in
            ( { model | outModel = outModel, input = input, error = error }, Cmd.none)
        MoveMsg msg_ ->
            let
                ( moveModel, _ ) = View.Terminal.Move.update msg_ model.moveModel
                input = if moveModel.result.msg == "OK" then "" else model.input
                error = moveModel.result.msg
            in
            ( { model | moveModel = moveModel, input = input, error = error }, Cmd.none)
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
        , div [] [ text model.error ]
        , div [] [ text model.input ]
        , div [] [ getCommandPanel model ]
        ]

getCommandPanel : Model -> Html msg
getCommandPanel model = 
    let
        command = getCommandName model
    in
    case command of
        None -> div [] [ text "non" ]
        Out -> View.Terminal.Out.view model.outModel model.acsModel model.input 
        Move -> View.Terminal.Move.view model.moveModel model.acsModel model.input 

getCommandSend : Model -> Cmd Msg
getCommandSend model =
    let
        command = getCommandName model
    in
    case command of
        None -> Cmd.none
        Out ->
            let
                cmd = View.Terminal.Out.getSendAction model.acsModel model.input 
            in
            Cmd.map OutMsg cmd
        Move ->
            let
                cmd = View.Terminal.Move.getSendAction model.acsModel model.input 
            in
            Cmd.map MoveMsg cmd
            

getCommandName : Model -> Command
getCommandName model =
    String.split " " model.input
    |> List.head
    |> Maybe.andThen getCommandType
    |> Maybe.withDefault None

getCommandType : String -> Maybe Command
getCommandType s =
    case s of
        "out" -> Just Out
        "move" -> Just Move
        _ -> Nothing


