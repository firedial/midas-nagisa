module Route exposing(Route(..), parse)

import Url
import Url.Parser

type Route
    = Top
    | Out

parse : Url.Url -> Maybe Route
parse url =
    Url.Parser.parse routing url

routing : Url.Parser.Parser (Route -> a) a
routing =
    Url.Parser.oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map Out (Url.Parser.s "out")
        ]


