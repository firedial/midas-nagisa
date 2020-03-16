module View.Terminal.Select exposing (view, getSelectAction, Msg, init, update, Model)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Balance
import Model.Attribute as Ma
import Request.GetBalance 
import Repository.AttributeCollection

type alias Model =
    { getBalanceModel : Request.GetBalance.Model
    }

type Msg 
    = GetBalance Request.GetBalance.Msg

init : ( Model, Cmd Msg )
init = 
    let
        ( model, cmd ) = Request.GetBalance.init
    in
    ( Model model, Cmd.map GetBalance cmd )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetBalance msg_ ->
            let
                ( getBalanceModel, _ ) = Request.GetBalance.update msg_ model.getBalanceModel
            in
            ( { model | getBalanceModel = getBalanceModel }, Cmd.none )

getSelectAction : Cmd Msg
getSelectAction = 
    let
        cmd = Request.GetBalance.get
    in
    Cmd.map GetBalance cmd
-- getSendAction : String -> Cmd msg
-- getSendAction s = Cmd.none

view : Model -> Html msg
view model =
    div [] 
        [ table []
            ([ tr []
                [ th [] [ text "balance_id" ]
                , th [] [ text "amount" ]
                , th [] [ text "item" ]
                , th [] [ text "kind_id" ]
                , th [] [ text "purpose_id" ]
                , th [] [ text "place_id" ]
                , th [] [ text "date" ]
                ]
            ]
            ++
            (List.map getRow model.getBalanceModel.balances)
            )
        ]

getRow : Model.Balance.Balance -> Html msg
getRow balance =
    tr []
        [ td [] [ text <| String.fromInt balance.balanceId ]
        , td [] [ text <| String.fromInt balance.amount ]
        , td [] [ text balance.item ]
        , td [] [ text <| String.fromInt balance.kindId ]
        , td [] [ text <| String.fromInt balance.purposeId ]
        , td [] [ text <| String.fromInt balance.placeId ]
        , td [] [ text balance.date ]
        ]

