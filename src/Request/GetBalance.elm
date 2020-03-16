module Request.GetBalance exposing (get, Msg, Model, init, update)

import Http exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Balance

type alias Model =
    { balances : Model.Balance.Balances
    }

type Msg 
    = GetBalances (Result Http.Error Model.Balance.Balances)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetBalances msg_ ->
            case msg_ of
                Ok balances ->
                    ( { model | balances = balances }, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )

init : ( Model, Cmd Msg )
init = ( Model [], Cmd.none )

get : Cmd Msg
get =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = getGetUrl
        , body = Http.emptyBody
        , expect = Http.expectJson GetBalances decodeBalances
        , timeout = Nothing
        , tracker = Nothing
        }

decodeBalances : Decode.Decoder Model.Balance.Balances
decodeBalances =
    list decodeBalance

decodeBalance : Decode.Decoder Model.Balance.Balance
decodeBalance =
    Decode.map7 Model.Balance.Balance
        (field "balance_id" Decode.int)
        (field "amount" Decode.int)
        (field "item" Decode.string)
        (field "kind_id" Decode.int)
        (field "purpose_id" Decode.int)
        (field "place_id" Decode.int)
        (field "date" Decode.string)

getGetUrl : String
getGetUrl = "http://localhost:3333/misuzu/api/v1/balance/"
