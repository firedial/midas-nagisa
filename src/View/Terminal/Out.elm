module View.Terminal.Out exposing (getView)

import Html exposing (..)
import Html.Attributes
import Html.Events

getView : String -> Html msg
getView s =
    div [] [ text s ]


