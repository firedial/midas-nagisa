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

type Command = None | Out

type Msg
    = Init
    | Input String
    | GetAttributes (Result Http.Error (List Model.Attribute.Attribute))
    | Send
    | Outet View.Terminal.Out.Msg

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
        Init ->
            ( { model | error = "init" }, Cmd.none)
        Send ->
            ( { model | error = "send" }, getCommandSend model)
        _ ->
            ( { model | error = "nothing" }, Cmd.none)

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
        Out -> View.Terminal.Out.getView model.kinds model.purposes model.places model.input 

getCommandSend : Model -> Cmd Msg
getCommandSend model =
    let
        command = getCommandName model
    in
    case command of
        None -> Cmd.none
        Out ->
            let
                cmd = View.Terminal.Out.getSendAction model.kinds model.purposes model.places model.input 
            in
            Cmd.map Outet cmd
            

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

