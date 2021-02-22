module Request.Attribute exposing (Model, Msg, init, update)

import Http exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Attribute 
import Config.Env

type alias Model =
    { status: String
    , errorMsg : String
    , attributes : Model.Attribute.Attributes
    }

type Msg 
    = GetAttributes (Result Http.Error Model)

init : String -> ( Model, Cmd Msg )
init attribute = 
    ( Model "" "" [], getAttributes attribute )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        GetAttributes result ->
            case result of
                Ok list ->
                    ( { model | attributes = list.attributes }, Cmd.none)
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

decodeAttributes : Decode.Decoder Model
decodeAttributes =
    Decode.map3 Model
        (field "status" Decode.string)
        (field "message" Decode.string)
        (field "data" (Decode.list decodeAttribute))

decodeAttribute : Decode.Decoder Model.Attribute.Attribute
decodeAttribute =
    Decode.map4 Model.Attribute.Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "category_id" Decode.int)

getGetUrl : String -> String
getGetUrl attribute = Config.Env.getApiUrl ++ "/" ++ attribute ++ "_elements/"
