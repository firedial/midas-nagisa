module Request.Util exposing (getErrMsg)

import Http exposing (..)

getErrMsg : Http.Error -> String
getErrMsg err =
    case err of
        BadUrl str ->
            "BadUrl: " ++ str
        Timeout ->
            "Timeout"
        NetworkError ->
            "NetworkError"
        BadStatus num ->
            "BadStatus: " ++ String.fromInt num
        BadBody str ->
            "BadBody" ++ str



