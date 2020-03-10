module Request.Attribute exposing (Model, Msg, init, update)

import Http exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Attribute 

type alias Model =
    { errorMsg : String
    , attributes : Model.Attribute.Attributes
    }

type Msg 
    = GetAttributes (Result Http.Error Model.Attribute.Attributes)

init : String -> ( Model, Cmd Msg )
init attribute = 
    ( Model "" [], getAttributes attribute )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        GetAttributes result ->
            case result of
                Ok list ->
                    ( { model | attributes = list }, Cmd.none)
                Err err ->
                    ( { model | errorMsg = "init api error" }, Cmd.none )

getAttributes : String -> Cmd Msg
getAttributes attribute =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = getGetUrl attribute
        , body = Http.emptyBody
        , expect = Http.expectJson GetAttributes decodeAttributes
        , timeout = Nothing
        , tracker = Nothing
        }

decodeAttributes : Decode.Decoder Model.Attribute.Attributes
decodeAttributes =
    Decode.list decodeAttribute

decodeAttribute : Decode.Decoder Model.Attribute.Attribute
decodeAttribute =
    Decode.map4 Model.Attribute.Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "group_id" Decode.int)

getGetUrl : String -> String
getGetUrl attribute = "http://localhost:3333/misuzu/api/v1/" ++ attribute ++ "/"
