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
import View.Terminal.Select
import View.Terminal.Sum
import View.Terminal.Salary
import Repository.AttributeCollection

type alias Model =
    { input : String
    , error : String
    , balance : Model.Balance.Balance
    , acsModel : Repository.AttributeCollection.Model
    , outModel : View.Terminal.Out.Model
    , moveModel : View.Terminal.Move.Model
    , selectModel : View.Terminal.Select.Model
    , sumModel : View.Terminal.Sum.Model
    , salaryModel : View.Terminal.Salary.Model
    }

init : ( Model, Cmd Msg )
init = 
    let
        ( outModel, _ ) = View.Terminal.Out.init
        ( moveModel, _ ) = View.Terminal.Move.init
        ( selectModel, _ ) = View.Terminal.Select.init
        ( sumModel, _ ) = View.Terminal.Sum.init
        ( salaryModel, _ ) = View.Terminal.Salary.init
        ( acsModel, cmd ) = Repository.AttributeCollection.init
    in
    ( Model "" "" Model.Balance.init acsModel outModel moveModel selectModel sumModel salaryModel, Cmd.map GetAttributeCollection cmd )

type Command = None | Out | Move | Select | Sum | Salary

type Msg
    = Send
    | Input String
    | GetAttributeCollection Repository.AttributeCollection.Msg
    | OutMsg View.Terminal.Out.Msg
    | MoveMsg View.Terminal.Move.Msg
    | SelectMsg View.Terminal.Select.Msg
    | SumMsg View.Terminal.Sum.Msg
    | SalaryMsg View.Terminal.Salary.Msg

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
        SelectMsg msg_ ->
            let
                ( selectModel, _ ) = View.Terminal.Select.update msg_ model.selectModel
                error = ""
            in
            ( { model | selectModel = selectModel, error = error }, Cmd.none)
        SumMsg msg_ ->
            let
                ( sumModel, _ ) = View.Terminal.Sum.update msg_ model.sumModel
                error = ""
            in
            ( { model | sumModel = sumModel, error = error }, Cmd.none)
        SalaryMsg msg_ ->
            let
                ( salaryModel, _ ) = View.Terminal.Salary.update msg_ model.salaryModel
                error = ""
            in
            ( { model | salaryModel = salaryModel, error = error }, Cmd.none)
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
        Select -> View.Terminal.Select.view model.selectModel 
        Sum -> View.Terminal.Sum.view model.sumModel 
        Salary -> View.Terminal.Salary.view model.salaryModel model.acsModel model.input 

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
        Select ->
            let
                cmd = View.Terminal.Select.getSelectAction model.input
            in
            Cmd.map SelectMsg cmd
        Sum ->
            let
                cmd = View.Terminal.Sum.getSelectAction model.input
            in
            Cmd.map SumMsg cmd
        Salary ->
            let
                cmd = View.Terminal.Salary.getSendAction model.acsModel model.input
            in
            Cmd.map SalaryMsg cmd
            

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
        "select" -> Just Select
        "sum" -> Just Sum
        "salary" -> Just Salary
        _ -> Nothing


