module Page.Terminal exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import String

import Model.Balance
import Model.Attribute 
import Config.Env

import View.Terminal.Out


type alias Model =
    { input : String
    , error : String
    , balance : Model.Balance.Balance
    , kinds : List Model.Attribute.Attribute
    , purposes : List Model.Attribute.Attribute
    , places : List Model.Attribute.Attribute
    }

init : ( Model, Cmd Msg )
init = ( Model "" "" Model.Balance.init [] [] [], getAttributes "kind" )

type CommandPanel = Out

type Msg
    = Init
    | Input String
    | Receive (Result Http.Error String)
    | GetAttributes (Result Http.Error (List Model.Attribute.Attribute))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            ( { model | input = str }, Cmd.none )
        GetAttributes result ->
            case result of
                Ok list ->
                    if model.kinds == [] then
                        ( { model | kinds = list }, getAttributes "purpose" )
                    else if model.purposes == [] then
                        ( { model | purposes = list }, getAttributes "place" )
                    else if model.places == [] then
                        ( { model | places = list }, Cmd.none)
                    else
                        ( model, Cmd.none )
                Err err ->
                    ( { model | error = "init api error" }, Cmd.none )
        Receive result ->
            case result of
                Ok str ->
                    case str of
                        "OK" ->
                            ( { model | balance = Model.Balance.init, error = "" }, Cmd.none)
                        _ ->
                            ( { model | error = "Error: " ++ str }, Cmd.none)
                Err err ->
                    ( { model | error = "post err" }, Cmd.none)
        Init ->
            ( { model | error = "init" }, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , div []
            [ input
                [ Html.Attributes.value <| model.input, Html.Events.onInput Input ]
                []
            ]
        , br [] []
        , text model.input
        , text model.error
        , getCommandPanel model.input
        ]

getCommandPanel : String -> Html msg
getCommandPanel str =
    let
        command = List.head <| String.split " " str
    in
    case command of
        Nothing ->
            div [] [ text "nothing" ]
        Just c ->
            case c of
                "out" -> View.Terminal.Out.getView str
                _ -> div [] [ text "non" ]

getAttributes : String -> Cmd Msg
getAttributes attribute =
    let
        url = Config.Env.getApiUrl ++ "/" ++ attribute ++ "/"
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson GetAttributes decodeAttributes
        , timeout = Nothing
        , tracker = Nothing
        }

decodeAttributes : Decode.Decoder (List Model.Attribute.Attribute)
decodeAttributes =
    Decode.list decodeAttribute

decodeAttribute : Decode.Decoder Model.Attribute.Attribute
decodeAttribute =
    Decode.map4 Model.Attribute.Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "group_id" Decode.int)

