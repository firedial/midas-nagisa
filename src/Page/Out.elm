module Page.Out exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Http 
import Json.Decode as Decode exposing (..)

import Model.Attribute 

type alias Model =
    { input : String
    , error : String
    , kinds : List Model.Attribute.Attribute
    , purposes : List Model.Attribute.Attribute
    , places : List Model.Attribute.Attribute
    }

init : ( Model, Cmd Msg )
init = ( Model "" "" [] [] [], getKindAttributes )

type Msg
    = Init
    | Input String
    | Send
    | Receive (Result Http.Error String)
    | GetAttributes (Result Http.Error (List Model.Attribute.Attribute))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        GetAttributes result ->
            case result of
                Ok list ->
                    if model.kinds == [] then
                        ( { model | kinds = list }, getPurposeAttributes)
                    else if model.purposes == [] then
                        ( { model | purposes = list }, getPlaceAttributes)
                    else if model.places == [] then
                        ( { model | places = list }, Cmd.none)
                    else
                        ( model, Cmd.none )
                Err err ->
                    ( { model | error = "init api error" }, Cmd.none )
        _ ->
            ( { model | input = "ng" }, Cmd.none)

view : Model -> Html Msg
view model =
    text model.input






getKindAttributes : Cmd Msg
getKindAttributes =
    getAttributes "kind"

getPurposeAttributes : Cmd Msg
getPurposeAttributes =
    getAttributes "purpose"

getPlaceAttributes : Cmd Msg
getPlaceAttributes =
    getAttributes "place"

getAttributes : String -> Cmd Msg
getAttributes attribute =
    let
        -- url = getDomain ++ "/api/v1/" ++ attribute ++ "/"
        url = "http://192.168.1.6:8080/api/v1/" ++ attribute ++ "/"
    in
        Http.get { url = url, expect = Http.expectJson GetAttributes decodeAttributes }

decodeAttributes : Decode.Decoder (List Model.Attribute.Attribute)
decodeAttributes =
    list decodeAttribute

decodeAttribute : Decode.Decoder Model.Attribute.Attribute
decodeAttribute =
    Decode.map4 Model.Attribute.Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "group_id" Decode.int)


