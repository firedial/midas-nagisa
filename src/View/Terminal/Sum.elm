module View.Terminal.Sum exposing (view, getSelectAction, Msg, init, update, Model)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Sum
import Model.Attribute as Ma
import Request.GetSum
import Repository.AttributeCollection

type alias Model =
    { getSumModel : Request.GetSum.Model
    }

type Msg 
    = GetSum Request.GetSum.Msg

init : ( Model, Cmd Msg )
init = 
    let
        ( model, cmd ) = Request.GetSum.init
    in
    ( Model model, Cmd.map GetSum cmd )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetSum msg_ ->
            let
                ( getSumModel, _ ) = Request.GetSum.update msg_ model.getSumModel
            in
            ( { model | getSumModel = getSumModel }, Cmd.none )

getSelectAction : String -> Cmd Msg
getSelectAction str = 
    let
        query = getQuery str
        cmd = Request.GetSum.get query
    in
    Cmd.map GetSum cmd

view : Model -> Html msg
view model =
    div [] 
        [ table []
            ([ tr []
                [ th [] [ text "id" ]
                , th [] [ text "amount_sum" ]
                , th [] [ text "date" ]
                ]
            ]
            ++
            (List.map getRow model.getSumModel.sums)
            )
        ]

getRow : Model.Sum.Sum -> Html msg
getRow sum =
    tr []
        [ td [] [ text <| String.fromInt sum.id ]
        , td [] [ text <| String.fromInt sum.amount_sum ]
        , td [] [ text sum.date ]
        ]

getQuery : String -> String
getQuery str =
    let
        strs = split " " str |> tail |> withDefault []
    in
    join "&" <| createQuery strs

createQuery : List String -> List String
createQuery strs =
    let
        key = head strs |> withDefault ""
        value = tail strs |> andThen head |> withDefault ""
        next = tail strs |> andThen tail |> withDefault []
    in
    if key == "" then [] else [(key ++ "=" ++ value)] ++ createQuery next



