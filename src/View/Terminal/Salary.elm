module View.Terminal.Salary exposing (view, getSendAction, Msg, init, update, Model)

import Html exposing (..)
import Html.Attributes
import Html.Events
import Http exposing (..)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)
import List exposing (head, tail, filter, map)

import Model.Balance
import Model.Attribute as Ma
import Request.PostBalance 
import Repository.AttributeCollection

type alias Model =
    { result : Request.PostBalance.Model
    , basicSalary : Int
    , coordinationSalary : Int
    , transportation : Int
    , shareBounty : Int
    , healthCostInsurance : Int
    , welfarePension : Int
    , residentTax : Int
    , employmentInsurance : Int
    , incomeTax : Int
    , share : Int
    }

type Msg 
    = OutPost Request.PostBalance.Msg

init : ( Model, Cmd Msg )
init = 
    let
        ( model, cmd ) = Request.PostBalance.init
    in
    ( Model model 0 0 0 0 0 0 0 0 0 0, Cmd.map OutPost cmd )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OutPost msg_ ->
            let
                ( result, _ ) = Request.PostBalance.update msg_ model.result
            in
            ( { model | result = result }, Cmd.none )

getSendAction : Repository.AttributeCollection.Model -> String -> Cmd Msg
getSendAction acs s = 
    Cmd.map OutPost Cmd.none
-- getSendAction : String -> Cmd msg
-- getSendAction s = Cmd.none

view : Model -> Repository.AttributeCollection.Model -> String -> Html msg
view model acs str =
    let
        data10 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        dataFromStr
            = split " " str
            |> tail
            |> withDefault [] 
            |> map toInt 
            |> map (withDefault 0)
        data = dataFromStr ++ data10
        names =
            [ "basicSalary"
            , "coordinationSalary"
            , "transportation"
            , "shareBounty"
            , "healthCostInsurance"
            , "welfarePension"
            , "residentTax"
            , "employmentInsurance"
            , "incomeTax" 
            , "share"
            ]
        zipped = List.map2 Tuple.pair names data
    in
    div [] (List.map getRow zipped)

getRow : (String, Int) -> Html msg
getRow (s, n) =
        div [] [ text <| s ++ ": " ++ String.fromInt n ]