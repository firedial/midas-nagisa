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
    , authToken : String
    }

type Panel
    = Amount
    | Item
    | Kind
    | Purpose
    | Place
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
                lastword = 
                    case panel of
                        Kind -> Util.Predictive.getWordWithDotCommand model.asc.kindCollection.attributeElements lastd
                        Purpose -> Util.Predictive.getWordWithDotCommand model.asc.purposeCollection.attributeElements lastd
                        Place -> Util.Predictive.getWordWithDotCommand model.asc.placeCollection.attributeElements lastd
                        _ -> lastd
                displayString = String.split " " str |> List.reverse |> List.tail |> Maybe.withDefault [] |> (::) lastword |> List.reverse |> String.join " "
            in
            ( { model | input = displayString }, Cmd.none )
        Send ->
            let
                balance = getBalanceFromString model.asc model.input
                cmd = postBalance model.authToken balance
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
        balance = getBalanceFromString acs str
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
        , Html.text "out"
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


postBalance : String -> Model.Balance.Balance -> Cmd Msg
postBalance authToken balance =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ authToken ) ]
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
        1 -> Amount
        2 -> Item
        3 -> Kind
        4 -> Purpose
        5 -> Place
        6 -> Date
        _ -> None
