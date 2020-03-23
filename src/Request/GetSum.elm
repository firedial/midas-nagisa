module Request.GetSum exposing (get, Msg, Model, init, update)

import Http exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Sum

type alias Model =
    { sums : Model.Sum.Sums
    }

type Msg 
    = GetSums (Result Http.Error Model.Sum.Sums)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSums msg_ ->
            case msg_ of
                Ok sums ->
                    ( { model | sums = sums }, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )

init : ( Model, Cmd Msg )
init = ( Model [], Cmd.none )

get : String -> Cmd Msg
get query =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = getGetUrl ++ "?" ++ query
        , body = Http.emptyBody
        , expect = Http.expectJson GetSums decodeSums
        , timeout = Nothing
        , tracker = Nothing
        }

decodeSums : Decode.Decoder Model.Sum.Sums
decodeSums =
    list decodeSum

decodeSum : Decode.Decoder Model.Sum.Sum
decodeSum =
    Decode.map3 Model.Sum.Sum
        (field "id" Decode.int)
        (field "amount_sum" Decode.int)
        (field "date" Decode.string)

getGetUrl : String
getGetUrl = "http://localhost:3333/misuzu/api/v1/sum/"
