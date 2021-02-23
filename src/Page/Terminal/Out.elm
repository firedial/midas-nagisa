module Page.Terminal.Out exposing (Model, Msg, init, update, view)

import Maybe 
import String
import List
import Http
import Html exposing (..)
import Html.Events
import Html.Attributes
import Request.Util
import Json.Decode
import Config.Env

import Model.Balance
import Model.Attribute
import Util.Predictive

type alias UpdateResponse =
    { status : String
    , message : String
    }

type alias Model =
    { asc : Model.Attribute.AttributesCollection
    , input : String
    , error : String
    }

type Panel
    = Amount
    | Item
    | Kind
    | Purpose
    | Place
    | Date
    | None

init : String -> ( Model, Cmd Msg )
init str = ( Model Model.Attribute.initAttributesCollection str "", Cmd.none )

type Msg
    = Send
    | Input String
    | Receive (Result Http.Error UpdateResponse)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            ( { model | input = str }, Cmd.none )
        Send ->
            let
                balance = String.split " " model.input |> List.tail |> Maybe.withDefault [] |> String.join " " |> getBalanceFromString model.asc
                cmd = postBalance balance
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

view : Model -> Html.Html Msg
view model =
    let
        acs = model.asc
        strd = model.input
        str = String.split "." strd |> List.head |> Maybe.withDefault ""
        balance = String.split " " str |> List.tail |> Maybe.withDefault [] |> String.join " " |> getBalanceFromString acs
        panel = getInputPanelName str
        last = String.split " " str |> List.reverse |> List.head |> Maybe.withDefault ""
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
        , Html.text "none"
        ]
        , div [] [Html.text model.error]
        , div [] [ Model.Balance.htmlMsg balance ]
        , br [] []
        , br [] []
        , case panel of
            Kind -> Util.Predictive.getPredictive acs.kindCollection.attributeElements last |> Util.Predictive.showAttributes
            Purpose -> Util.Predictive.getPredictive acs.purposeCollection.attributeElements last |> Util.Predictive.showAttributes
            Place -> Util.Predictive.getPredictive acs.placeCollection.attributeElements last |> Util.Predictive.showAttributes
            _ -> div [] []
        ]


postBalance : Model.Balance.Balance -> Cmd Msg
postBalance balance =
    Http.request
        { method = "POST"
        , headers = []
        , url = Config.Env.getApiUrl ++ "/balance/"
        , body = Model.Balance.encodeBalance balance |> Http.jsonBody
        , expect = Http.expectJson Receive decodeUpdateResponse
        , timeout = Nothing
        , tracker = Nothing
        }

decodeUpdateResponse : Json.Decode.Decoder UpdateResponse
decodeUpdateResponse =
    Json.Decode.map2 UpdateResponse
        (Json.Decode.field "status" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)

getBalanceFromString : Model.Attribute.AttributesCollection -> String -> Model.Balance.Balance
getBalanceFromString acs str =
    let
        kinds = acs.kindCollection.attributeElements
        purposes = acs.purposeCollection.attributeElements
        places = acs.placeCollection.attributeElements
        strs = String.split " " str

        amount = List.head strs |> Maybe.andThen String.toInt |> Maybe.withDefault 0 |> (*) -1
        amountTail = List.tail strs |> Maybe.withDefault []

        item = List.head amountTail |> Maybe.withDefault ""
        itemTail = List.tail amountTail |> Maybe.withDefault []

        kind = List.head itemTail |> Maybe.withDefault "" |> getAttributeId kinds
        kindTail = List.tail itemTail |> Maybe.withDefault []

        purpose = List.head kindTail |> Maybe.withDefault "" |> getAttributeId purposes 
        purposeTail = List.tail kindTail |> Maybe.withDefault []

        place = List.head purposeTail |> Maybe.withDefault "" |> getAttributeId places
        placeTail = List.tail purposeTail |> Maybe.withDefault []

        date = List.head placeTail |> Maybe.withDefault ""
    in
    Model.Balance.Balance 0 amount item kind purpose place date


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
        2 -> Amount
        3 -> Item
        4 -> Kind
        5 -> Purpose
        6 -> Place
        7 -> Date
        _ -> None
