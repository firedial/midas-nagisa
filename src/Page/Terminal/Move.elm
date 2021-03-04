module Page.Terminal.Move exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http
import Maybe
import String
import List
import Json.Decode
import Request.Util

import Config.Env
import Model.Move
import Model.Attribute
import Repository.AttributeCollection
import Util.Predictive

type alias UpdateResponse =
    { status : String
    , message : String
    }

type alias Model =
    { asc : Model.Attribute.AttributesCollection
    , input : String
    , error : String
    , authToken : String
    }

type Panel
    = Attribute
    | Amount
    | Before
    | After
    | Date
    | None

init : String -> Model.Attribute.AttributesCollection -> ( Model, Cmd Msg )
init authToken asc = ( Model asc "" "" authToken, Cmd.none )

type Msg
    = Send
    | Input String
    | Receive (Result Http.Error UpdateResponse)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input strd ->
            let
                str = String.split "." strd |> List.head |> Maybe.withDefault ""
                panel = getInputPanelName str
                lastd = String.split " " strd |> List.reverse |> List.head |> Maybe.withDefault ""
                attributeName = strd |> String.split " " |> List.head |> Maybe.withDefault ""
                attributes = 
                    case attributeName of
                        "purpose" -> model.asc.purposeCollection.attributeElements
                        "place" -> model.asc.placeCollection.attributeElements
                        _ -> []
                lastword =
                    case panel of
                        Before -> Util.Predictive.getWordWithDotCommand attributes lastd
                        After -> Util.Predictive.getWordWithDotCommand attributes lastd
                        _ -> lastd
                displayString = String.split " " str |> List.reverse |> List.tail |> Maybe.withDefault [] |> (::) lastword |> List.reverse |> String.join " "
            in
            ( { model | input = displayString }, Cmd.none )
        Send ->
            let
                move = getMoveFromString model.asc model.input
                cmd = postMove model.authToken move
            in
            ( model, cmd )
        Receive result ->
            case result of
                Ok response ->
                    let
                        input = if response.status == "OK" then "" else model.input
                        message = if response.status == "OK" then response.message else ""
                    in
                    ( { model | input = input, error = message }, Cmd.none )
                Err err ->
                    ( { model | error = Request.Util.getErrMsg err }, Cmd.none )

view : Model -> Html Msg
view model =
    let
        acs = model.asc
        strd = model.input
        str = String.split "." strd |> List.head |> Maybe.withDefault ""
        last = String.split " " str |> List.reverse |> List.head |> Maybe.withDefault ""
        move = getMoveFromString acs str
        panel = getInputPanelName str
        attributeName = strd |> String.split " " |> List.head |> Maybe.withDefault ""
        attributes = 
            case attributeName of
                "purpose" -> acs.purposeCollection.attributeElements
                "place" -> acs.placeCollection.attributeElements
                _ -> []
    in
    div [] 
        [
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
        , Html.text "move"
        ]
        , div [] [Html.text model.error]
        , div [] [ Model.Move.htmlMsg move ]
        , case panel of
            Before -> Util.Predictive.getPredictive attributes last |> Util.Predictive.showAttributes
            After -> Util.Predictive.getPredictive attributes last |> Util.Predictive.showAttributes
            _ -> div [] []
        ]

getMoveFromString : Model.Attribute.AttributesCollection -> String -> Model.Move.Move
getMoveFromString acs str =
    let
        strs = String.split " " str

        attribute = List.head strs |> Maybe.withDefault ""
        attributeTail = List.tail strs |> Maybe.withDefault []

        attributes =
            case attribute of
                "purpose" -> 
                    acs.purposeCollection.attributeElements
                "places" -> 
                    acs.placeCollection.attributeElements
                _ ->
                    acs.placeCollection.attributeElements

        amount = List.head attributeTail |> Maybe.andThen String.toInt |> Maybe.withDefault 0 
        amountTail = List.tail attributeTail |> Maybe.withDefault []

        beforeId = List.head amountTail |> Maybe.withDefault "" |> getAttributeId attributes
        beforeTail = List.tail amountTail |> Maybe.withDefault []

        afterId = List.head beforeTail |> Maybe.withDefault "" |> getAttributeId attributes
        afterTail = List.tail beforeTail |> Maybe.withDefault []

        date = List.head afterTail |> Maybe.withDefault ""
    in
    Model.Move.Move attribute amount beforeId afterId date

getAttributeId : Model.Attribute.AttributeElements -> String -> Int
getAttributeId attributes str =
    List.filter (\n -> n.name == str) attributes
    |> List.map (\n -> n.id)
    |> List.head 
    |> Maybe.withDefault 0

getInputPanelName : String -> Panel
getInputPanelName str =
    let
        len = String.split " " str |> List.length
    in
    case len of
        1 -> Attribute
        2 -> Amount
        3 -> Before
        4 -> After
        5 -> Date
        _ -> None

postMove : String -> Model.Move.Move -> Cmd Msg
postMove authToken move =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ authToken ) ]
        , url = Config.Env.getApiUrl ++ "/move/"
        , body = Model.Move.encodeMove move |> Http.jsonBody
        , expect = Http.expectJson Receive decodeUpdateResponse
        , timeout = Nothing
        , tracker = Nothing
        }

decodeUpdateResponse : Json.Decode.Decoder UpdateResponse
decodeUpdateResponse =
    Json.Decode.map2 UpdateResponse
        (Json.Decode.field "status" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)


