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
import Repository.AttributeCollection

type alias Model =
    { input : String
    , error : String
    , balance : Model.Balance.Balance
    , acsModel : Repository.AttributeCollection.Model
    , outModel : View.Terminal.Out.Model
    }

init : ( Model, Cmd Msg )
init = 
    let
        ( outModel, _ ) = View.Terminal.Out.init
        ( acsModel, cmd ) = Repository.AttributeCollection.init
    in
    ( Model "" "" Model.Balance.init acsModel outModel, Cmd.map GetAttributeCollection cmd )

type Command = None | Out

type Msg
    = Send
    | Input String
    | GetAttributeCollection Repository.AttributeCollection.Msg
    | OutMsg View.Terminal.Out.Msg

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
            in
            ( { model | outModel = outModel }, Cmd.none)
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
        , text model.input
        , text model.error
        , getCommandPanel model
        ]

getCommandPanel : Model -> Html msg
getCommandPanel model = 
    let
        command = getCommandName model
    in
    case command of
        None -> div [] [ text "non" ]
        Out -> View.Terminal.Out.view model.outModel model.acsModel model.input 

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
        _ -> Nothing


