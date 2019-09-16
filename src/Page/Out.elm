module Page.Out exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http 
import Json.Decode as Decode exposing (..)

import Model.Attribute 
import Model.Balance

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
    | AttributeAction String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        Input str ->
            ( { model | input = str }, Cmd.none )
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
        AttributeAction atr name ->
            case atr of
                "kind" ->
                    ( { model | input = replace 2 model.input name }, Cmd.none)
                "purpose" ->
                    ( { model | input = replace 3 model.input name }, Cmd.none)
                "place" ->
                    ( { model | input = replace 4 model.input name }, Cmd.none)
                _ ->
                    ( model , Cmd.none)


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
        , Model.Balance.htmlMsg ( Model.Balance.interpretation model.kinds model.purposes model.places model.input )
        , br [] []
        , text model.input
        , ul [] (List.map ( viewAttributes "kind" ) model.kinds )
        , br [] []
        , ul [] (List.map ( viewAttributes "purpose" ) model.purposes)
        , br [] []
        , ul [] (List.map ( viewAttributes "place" ) model.places)
        ]



viewAttributes : String -> Model.Attribute.Attribute -> Html Msg
viewAttributes name atr =
    li [] 
        [ button 
            [ Html.Attributes.class "aaaa"
            , Html.Events.onClick ( AttributeAction name atr.name )
            ]
            [ text atr.name ]
        ]



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

replace : Int -> String -> String -> String
replace n input name =
    String.join " " ( replaceLoop n name (String.split " " input) )

replaceLoop : Int -> String -> List String -> List String
replaceLoop n name inputList =
    if n < 0 then
        inputList
    else if n == 0 then
        case inputList of
            [] ->
                [name]
            (x::xs) -> 
                name :: ( replaceLoop ( n - 1 ) name xs )
    else
        case inputList of
            [] ->
                "" :: ( replaceLoop ( n - 1 ) name [] )
            (x::xs) -> 
                x :: ( replaceLoop ( n - 1 ) name xs )




