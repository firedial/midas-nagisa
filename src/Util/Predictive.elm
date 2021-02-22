module Util.Predictive exposing (getWordWithDotCommand, showAttributes, getPredictive)

import Html exposing (..)
import List exposing (head, tail, filter)
import Maybe exposing (andThen, withDefault)
import String exposing (split, join, toInt)

import Model.Attribute as Ma

getSamePrefixString : String -> List String -> String
getSamePrefixString str strings =
    let
        stringHead = List.map (String.left 1) strings
        stringTail = List.map (String.dropLeft 1) strings
        startString = stringHead |> head |> withDefault ""
        s = List.foldl (\x y -> if x /= y then "" else x) startString stringHead
    in
    if s == "" then str else getSamePrefixString (str ++ s) stringTail

showAttributes : Ma.AttributeElements -> Html msg
showAttributes attributes =
    let
        predictiveList = List.take 8 attributes
        strings = List.map (\a -> a.name) predictiveList
        sameString = getSamePrefixString "" strings
        c = [".", "f", "j", "g", "h", "d", "k", "s", "l"]
        l = List.map2 (\x y -> (x, y)) c (sameString :: strings)
    in
    div [] ( List.map (\s -> div [] [ text (Tuple.first s ++ ": " ++ Tuple.second s) ]) l )

getWordWithDotCommand : Ma.AttributeElements -> String -> String
getWordWithDotCommand attributes wordd =
    let
        c = if String.contains ".." wordd then "." else split "." wordd |> tail |> withDefault [] |> head |> withDefault ""
        word = split "." wordd |> head |> withDefault ""
        num = convertDotCommandToNumber c
        predictives = getPredictive attributes word |> List.map (\x -> x.name)
    in
    if c == "." then
        getSamePrefixString "" predictives
    else
        case num of
            Nothing -> wordd
            Just n ->
                let
                    w = List.drop n predictives |> head
                in
                case w of
                    Just w_ -> w_ ++ " "
                    Nothing -> ""

getPredictive : Ma.AttributeElements -> String -> Ma.AttributeElements
getPredictive attributes str =
    filter (\n -> String.startsWith str n.name) attributes

convertDotCommandToNumber : String -> Maybe Int
convertDotCommandToNumber c =
    case c of
        "f" -> Just 0
        "j" -> Just 1
        "g" -> Just 2 
        "h" -> Just 3 
        "d" -> Just 4 
        "k" -> Just 5 
        "s" -> Just 6
        "l" -> Just 7 
        _ -> Nothing
