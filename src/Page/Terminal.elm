module Page.Terminal exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Model.Balance
import Model.Attribute 
import Config.Env


type alias Model =
    { input : String
    , error : String
    , balance : Model.Balance.Balance
    , panel : Panel
    , kinds : List Model.Attribute.Attribute
    , purposes : List Model.Attribute.Attribute
    , places : List Model.Attribute.Attribute
    }

init : ( Model, Cmd Msg )
init = ( Model "" "" Model.Balance.init Amount [] [] [], getAttributes "kind" )

type Direction = Back | Next

type Panel = Amount | Item | Kind | Purpose | Place | Date | Confirm

type Msg
    = Init
    | Input String
    | Send
    | Receive (Result Http.Error String)
    | GetAttributes (Result Http.Error (List Model.Attribute.Attribute))
    | AttributeAction String
    | Panel Direction

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
        Send ->
            ( model, Model.Balance.encode model.balance |> balancePost)
        Receive result ->
            case result of
                Ok str ->
                    case str of
                        "OK" ->
                            ( { model | balance = Model.Balance.init, panel = Amount, error = "" }, Cmd.none)
                        _ ->
                            ( { model | error = "Error: " ++ str }, Cmd.none)
                Err err ->
                    ( { model | error = "post err" }, Cmd.none)
        Init ->
            ( { model | error = "init" }, Cmd.none)
        AttributeAction value ->
            case model.panel of
                Amount ->
                    let 
                        balance = model.balance
                        newBalance = { balance | amount = -1 * getIntFromString model.input }
                    in
                    ( { model
                        | balance = newBalance
                        , panel = getNextPanelName model.panel 
                        , input = ""
                    }, Cmd.none )
                Item ->
                    let 
                        balance = model.balance
                        newBalance = { balance | item = model.input }
                    in
                    ( { model
                        | balance = newBalance
                        , panel = getNextPanelName model.panel 
                        , input = ""
                    }, Cmd.none )
                Date ->
                    let 
                        balance = model.balance
                        newBalance = { balance | date = model.input }
                    in
                    ( { model
                        | balance = newBalance
                        , panel = getNextPanelName model.panel 
                        , input = ""
                    }, Cmd.none )
                _ -> 
                    ( { model | balance = setBalance model.balance model.panel value, panel = getNextPanelName model.panel }, Cmd.none)
        Panel d ->
            case d of
                Back -> 
                    ( { model | panel = getBackPanelName model.panel }, Cmd.none)
                Next -> 
                    ( { model | panel = getNextPanelName model.panel }, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , text model.input
        , br [] []
        , div [] [ button [ Html.Events.onClick ( Panel Back ) ] [ text "back" ] ]
        , div [] [ button [ Html.Events.onClick ( Panel Next ) ] [ text "next" ] ]
        , div []
            [ p [] [ text <| getPanelName model.panel ]
            , getPanelView model 
            ]
        , br [] []
        , br [] []
        , Model.Balance.htmlMsg model.balance
        , br [] []
        , text model.error
        ]



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




-- Panel

viewAttributes : Panel -> Model.Attribute.Attribute -> Html Msg
viewAttributes panel atr =
    li [] 
        [ button 
            [ Html.Attributes.class "aaaa"
            , Html.Events.onClick ( AttributeAction ( String.fromInt atr.id ) )
            ]
            [ text atr.name ]
        ]

setBalance : Model.Balance.Balance -> Panel -> String -> Model.Balance.Balance
setBalance balance panel value =
    case panel of
        Kind -> { balance | kindId = getIntFromString value }
        Purpose -> { balance | purposeId = getIntFromString value }
        Place -> { balance | placeId = getIntFromString value }
        _ -> balance


getPanelView : Model -> Html Msg
getPanelView model =
    case model.panel of
        Amount ->
            div []
                [ Html.form
                    [ Html.Events.onSubmit <| AttributeAction <| String.fromInt model.balance.amount ]
                    [ input
                        [ Html.Attributes.value <| model.input, Html.Events.onInput Input ]
                        []
                    ]
                    , button [ Html.Attributes.disabled False ]
                    [ text "Submit" ]
                ]
        Item ->
            div []
                [ Html.form
                    [ Html.Events.onSubmit <| AttributeAction model.balance.item ]
                    [ input
                        [ Html.Attributes.value model.input, Html.Events.onInput Input ]
                        []
                    ]
                    , button []
                    [ text "Submit" ]
                ]
        Kind ->
            ul [] ( List.map ( viewAttributes Kind ) model.kinds )
        Purpose ->
            ul [] ( List.map ( viewAttributes Purpose ) model.purposes )
        Place ->
            ul [] ( List.map ( viewAttributes Place ) model.places )
        Date ->
            div []
                [ Html.form
                    [ Html.Events.onSubmit <| AttributeAction model.balance.date ]
                    [ input
                        [ Html.Attributes.value model.input, Html.Events.onInput Input ]
                        []
                    ]
                    , button []
                    [ text "Submit" ]
                ]
        Confirm ->
            button 
                [ Html.Attributes.class "aaaa"
                    , Html.Events.onClick Send 
                ]
                [ text "Submit" ]

getNextPanelName : Panel -> Panel
getNextPanelName p =
    case p of
        Amount -> Item
        Item -> Kind
        Kind -> Purpose
        Purpose -> Place
        Place -> Date
        Date -> Confirm
        Confirm -> Confirm

getBackPanelName : Panel -> Panel
getBackPanelName p =
    case p of
        Amount -> Amount
        Item -> Amount
        Kind -> Item
        Purpose -> Kind
        Place -> Purpose
        Date -> Place
        Confirm -> Date

getIntFromString : String -> Int
getIntFromString str =
    let
        maybeInt = String.toInt str
    in
    case maybeInt of
        Nothing -> 0
        Just n -> n


-- Send

balancePost : Encode.Value -> Cmd Msg
balancePost = Config.Env.getApiUrl ++ "/balance/" |> post

post : String -> Encode.Value -> Cmd Msg
post url encode =
    let
        body = encode |> Http.jsonBody
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ( "Bearer " ++ "token" ) ]
        , url = url
        , body = body
        , expect = Http.expectJson Receive Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }

-- util

getPanelName : Panel -> String
getPanelName panel =
    case panel of
        Amount -> "Amount"
        Item -> "Item"
        Kind -> "Kind"
        Purpose -> "Purpose"
        Place -> "Place"
        Date -> "Date"
        Confirm -> "Confirm"


