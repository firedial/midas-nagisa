module Route exposing(Route(..), parse)

import Url
import Url.Parser exposing (..)

type Route
    = Top
    | Terminal

parse : Url.Url -> Maybe Route
parse url =
    Url.Parser.parse routing url

routing : Url.Parser.Parser (Route -> a) a
routing =
    Url.Parser.oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map Terminal (Url.Parser.s "nagisa" </> Url.Parser.s "terminal")
        ]


