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
        [ ul []
            (List.map (\l -> li [] [ text "s" ]) model.getBalanceModel.balances)
        ]

