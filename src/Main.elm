module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes
import Http
import Route
import Url
import Url.Builder

import Page.Top
import Page.Terminal

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = UrlRequested
    }

type alias Model =
    { key : Nav.Key
    , page : Page
    }

type Page
    = NotFound
    | Top Page.Top.Model
    | Terminal Page.Terminal.Model

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key ( Top ( Page.Top.Model "aaa" 1 ) )
    |> goTo (Route.parse url)

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | TopMsg Page.Top.Msg
    | TerminalMsg Page.Terminal.Msg
    | Loaded (Result Http.Error Page)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            goTo (Route.parse url) model
        Loaded result ->
            ( { model
                | page =
                    case result of
                        Ok page ->
                            page
                        _ ->
                            NotFound
                }
            , Cmd.none    
            )
        TopMsg topMsg ->
            case model.page of
                Top topModel ->
                    let 
                        ( newModel, newCmd ) =
                            Page.Top.update topMsg topModel
                    in
                        ( { model | page = Top newModel }
                        , Cmd.map TopMsg newCmd
                        )
                _ ->
                    ( model, Cmd.none )
        TerminalMsg terminalMsg ->
            case model.page of
                Terminal terminalModel ->
                    let 
                        ( newModel, newCmd ) =
                            Page.Terminal.update terminalMsg terminalModel
                    in
                    ( { model | page = Terminal newModel }
                    , Cmd.map TerminalMsg newCmd
                    )
                _ ->
                    ( model, Cmd.none )

goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
        Just Route.Top ->
            let
                ( topModel, topCmd ) =
                    Page.Top.init "aaa" 1
            in
            ( { model | page = Top topModel }
            , Cmd.map TopMsg topCmd
            )
        Just Route.Terminal ->
            let
                ( terminalModel, terminalCmd ) =
                    Page.Terminal.init 
            in
            ( { model | page = Terminal terminalModel }
            , Cmd.map TerminalMsg terminalCmd
            )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Browser.Document Msg
view model =
    { title = "midas"
    , body = 
        [ Html.a [Html.Attributes.href "/nagisa"] [Html.text "top"]
        , Html.br [] []
        , Html.a [Html.Attributes.href "/nagisa/terminal"] [Html.text "terminal"]
        , Html.br [] []
        , case model.page of
            NotFound ->
                viewNotFound
            Top topModel ->
                Page.Top.view topModel
                    |> Html.map TopMsg
            Terminal terminalModel ->
                Page.Terminal.view terminalModel
                    |> Html.map TerminalMsg
        ]
    }

viewNotFound : Html.Html msg
viewNotFound = 
    Html.text "not found"
    
viewTop : Html.Html msg
viewTop = 
    Html.text "top"

viewTerminal : Html.Html msg
viewTerminal = 
    Html.text "terminal"