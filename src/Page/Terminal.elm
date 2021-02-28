module Page.Terminal exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http 
import Json.Encode
import Json.Decode
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)


import Model.Balance
import Model.Move
import Model.Attribute
import Config.Env

import Page.Terminal.Out
import Page.Terminal.Move
import Repository.AttributeCollection
import Request.Util

type alias Model =
    { command : Command
    , asc : Model.Attribute.AttributesCollection
    , input : String
    , error : String
    }

type alias GetAttributeElementsResponse =
    { status : String
    , message : String
    , data : Model.Attribute.AttributeElements
    }

type Command
    = None
    | Out Page.Terminal.Out.Model
    | Move Page.Terminal.Move.Model

init : ( Model, Cmd Msg )
init = 
    ( Model None Model.Attribute.initAttributesCollection "" ""
    , Cmd.batch
        [ getAttributeElements "kind"
        , getAttributeElements "purpose"
        , getAttributeElements "place"
        ]
    )

type Msg
    = Send
    | ChangeCommand String
    | GetAttributeElements String (Result Http.Error GetAttributeElementsResponse)
    | Receive (Result Http.Error String)
    | OutMsg Page.Terminal.Out.Msg
    | MoveMsg Page.Terminal.Move.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ChangeCommand str ->
            case str of
                "out" ->
                    let
                        ( outModel, _ ) = Page.Terminal.Out.init model.asc
                    in
                    ( { model | command = Out outModel, input = str }, Cmd.none )
                "move" ->
                    let
                        ( moveModel, _ ) = Page.Terminal.Move.init model.asc
                    in
                    ( { model | command = Move moveModel, input = str }, Cmd.none )
                _ ->
                    ( { model | input = str }, Cmd.none )
        Send ->
            ( model, Cmd.none )
        GetAttributeElements attibuteName result ->
                case result of
                    Ok response ->
                        case response.status of
                            "OK" ->
                                ( { model 
                                    | input = ""
                                    , error = ""
                                    , asc = getAttributesCollectionUpdatedAttributeElements
                                        model.asc
                                        attibuteName
                                        response.data
                                }
                                , Cmd.none )
                            _ ->
                                ( { model | error = response.message }, Cmd.none )
                    Err err ->
                        ( { model | error = Request.Util.getErrMsg err }, Cmd.none )
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
        MoveMsg msg_ ->
            case model.command of
                Move model_ ->
                    let 
                        ( newModel, newCmd ) =
                            Page.Terminal.Move.update msg_ model_
                    in
                    ( { model | command = Move newModel }
                    , Cmd.map MoveMsg newCmd
                    )
                _ ->
                    ( model, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , input
            [ id "out"
            , name "command"
            , type_ "radio"
            , value "out"
            , onChange ChangeCommand
            ] []
        , label [ for "out" ] [ text "Out" ]
        , input
            [ id "move"
            , name "command"
            , type_ "radio"
            , value "move"
            , onChange ChangeCommand
            ] []
        , label [ for "move" ] [ text "Move" ]
        , div []
            [
                case model.command of
                    None ->
                        div [] []
                    Out model_ ->
                        Page.Terminal.Out.view model_ |> Html.map OutMsg
                    Move model_ ->
                        Page.Terminal.Move.view model_ |> Html.map MoveMsg
            ]
        ]


getAttributeElements : String -> Cmd Msg
getAttributeElements attribute =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = Config.Env.getApiUrl ++ "/" ++ attribute ++ "_elements/"
        , body = Http.emptyBody
        , expect = Http.expectJson (GetAttributeElements attribute) decodeAttributeElementsResoponse
        , timeout = Nothing
        , tracker = Nothing
        }

decodeAttributeElementsResoponse : Json.Decode.Decoder GetAttributeElementsResponse
decodeAttributeElementsResoponse =
    Json.Decode.map3 GetAttributeElementsResponse
        (Json.Decode.field "status" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)
        (Json.Decode.field "data" Model.Attribute.decodeAttributeElements)

getAttributesCollectionUpdatedAttributeElements : Model.Attribute.AttributesCollection -> String -> Model.Attribute.AttributeElements -> Model.Attribute.AttributesCollection
getAttributesCollectionUpdatedAttributeElements asc attributeName attributeElements =
    case attributeName of
        "kind" ->
            let
                attributeCollection = asc.kindCollection
                newAttributeCollection = { attributeCollection | attributeElements = attributeElements }
            in
            { asc | kindCollection = newAttributeCollection }
        "purpose" ->
            let
                attributeCollection = asc.purposeCollection
                newAttributeCollection = { attributeCollection | attributeElements = attributeElements }
            in
            { asc | purposeCollection = newAttributeCollection }
        "place" ->
            let
                attributeCollection = asc.placeCollection
                newAttributeCollection = { attributeCollection | attributeElements = attributeElements }
            in
            { asc | placeCollection = newAttributeCollection }
        _ ->
            asc


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger Html.Events.targetValue)
