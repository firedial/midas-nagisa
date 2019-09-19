module Page.Out exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Decode as Decode exposing (..)

import Model.Balance
import Model.Attribute 


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

type Panel = Amount | Item | Kind | Purpose | Place | Date

type Msg
    = Init
    | Input String
    | Send
    | Receive (Result Http.Error String)
    | GetAttributes (Result Http.Error (List Model.Attribute.Attribute))
    | AttributeAction Panel String
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
            ( { model | error = "send" }, Cmd.none)
        Receive result ->
            case result of
                Ok str ->
                    ( { model | error = "ok" }, Cmd.none)
                Err err ->
                    ( { model | error = "err" }, Cmd.none)
        Init ->
            ( { model | error = "init" }, Cmd.none)
        AttributeAction panel value ->
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
        [ Html.form [ Html.Events.onSubmit Send ]
            [ input [ Html.Attributes.value model.input, Html.Events.onInput Input ] []
            , button
                [ Html.Attributes.disabled (String.length model.input < 1) ]
                [ text "Submit" ]
            ]
        , br [] []
        , text model.error
        , br [] []
        , Model.Balance.htmlMsg model.balance
        , br [] []
        , text model.input
        , br [] []
        , br [] []
        , div [] [ button [ Html.Events.onClick ( Panel Back ) ] [ text "back" ] ]
        , div [] [ button [ Html.Events.onClick ( Panel Next ) ] [ text "next" ] ]
        , div [] [ getPanelView model ]
        ]



getAttributes : String -> Cmd Msg
getAttributes attribute =
    let
        -- url = getDomain ++ "/api/v1/" ++ attribute ++ "/"
        url = "http://localhost:8080/api/v1/" ++ attribute ++ "/"
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




-- Panel

viewAttributes : Panel -> Model.Attribute.Attribute -> Html Msg
viewAttributes panel atr =
    li [] 
        [ button 
            [ Html.Attributes.class "aaaa"
            , Html.Events.onClick ( AttributeAction panel ( String.fromInt atr.id ) )
            ]
            [ text atr.name ]
        ]

setBalance : Model.Balance.Balance -> Panel -> String -> Model.Balance.Balance
setBalance balance panel value =
    case panel of
        Amount -> { balance | amount = getIntFromString value }
        Item -> { balance | item = value }
        Kind -> { balance | kindId = getIntFromString value }
        Purpose -> { balance | purposeId = getIntFromString value }
        Place -> { balance | placeId = getIntFromString value }
        Date -> { balance | date = value }

getPanelView : Model -> Html Msg
getPanelView model =
    case model.panel of
        Kind ->
            ul [] ( List.map ( viewAttributes Kind ) model.kinds )
        Purpose ->
            ul [] ( List.map ( viewAttributes Purpose ) model.purposes )
        Place ->
            ul [] ( List.map ( viewAttributes Place ) model.places )
        Date ->
            div []
                [ button 
                    [ Html.Attributes.class "aaaa"
                    , Html.Events.onClick ( AttributeAction Date "today" )
                    ]
                    [ text "today" ]
                ]
        _ ->
            div [] []
            --     [ Html.form [ Html.Events.onSubmit AttributeAction model.panel ]
            --         [ input [ Html.Attributes.value model.input ] []
            --         , button
            --         [ Html.Attributes.disabled (String.length model.input < 1) ]
            --         [ text "Submit" ]
            --     ]

getNextPanelName : Panel -> Panel
getNextPanelName p =
    case p of
        Amount -> Item
        Item -> Kind
        Kind -> Purpose
        Purpose -> Place
        Place -> Date
        Date -> Date

getBackPanelName : Panel -> Panel
getBackPanelName p =
    case p of
        Amount -> Amount
        Item -> Amount
        Kind -> Item
        Purpose -> Kind
        Place -> Purpose
        Date -> Place

getPanelNumber : Panel -> Int
getPanelNumber p = 
    case p of
        Amount -> 0
        Item -> 1
        Kind -> 2
        Purpose -> 3
        Place -> 4
        Date -> 5

getIntFromString : String -> Int
getIntFromString str =
    let
        maybeInt = String.toInt str
    in
    case maybeInt of
        Nothing -> 0
        Just n -> n


