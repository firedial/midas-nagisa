module Model.Attribute exposing (Attribute, Attributes, Kinds, Purposes, Places, decodeAttributes, resultAttributes, getAttributeNumber)

import Json.Decode as Decode exposing (..)

type alias Attributes = List Attribute
type alias Kinds = List Attribute
type alias Purposes = List Attribute
type alias Places = List Attribute

type alias Attribute =
    { id : Int
    , name : String
    , description : String
    , group_id : Int
    }

resultAttributes : Result Error (List Attribute) -> List Attribute
resultAttributes result =
    case result of
        Ok attributes ->
            attributes
        _ ->
            []


-- decodeAttributes : String -> Result Error (List Attribute)
-- decodeAttributes str =
--     decodeString (list decodeAttribute) str

decodeAttributes : Decode.Decoder (List Attribute)
decodeAttributes =
    list decodeAttribute

decodeAttribute : Decode.Decoder Attribute
decodeAttribute =
    Decode.map4 Attribute
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "description" Decode.string)
        (field "group_id" Decode.int)

getAttributeNumber : List Attribute -> Maybe String -> Maybe Int
getAttributeNumber attributes str =
    case str of
        Nothing ->
            Nothing
        Just s ->
            let
                attribute = List.filter (\n -> n.name == s) attributes
            in
                case (List.head attribute) of
                    Just f ->
                        Maybe.Just f.id
                    Nothing ->
                        Nothing

getAttributeNumberExpect : List Attribute -> Maybe String -> Maybe Int
getAttributeNumberExpect attributes str =
    case str of
        Nothing ->
            Nothing
        Just s ->
            let 
                list = enableAttribute attributes s     
                number = case (List.head list) of
                    Just f ->
                        Maybe.Just f.id
                    Nothing ->
                        Nothing
            in
                if List.length list == 1 then
                    number
                else
                    Nothing
        
enableAttribute : List Attribute -> String -> List Attribute
enableAttribute attributes str =
    List.filter (isStartStringAttribute str) attributes

isStartStringAttribute : String -> Attribute -> Bool
isStartStringAttribute splitString attribute =
    isStartString splitString attribute.name

isStartString : String -> String -> Bool
isStartString splitString separatedString =
    let
        str = String.split splitString separatedString |> List.head
    in
        case str of
            Just s ->
                if s == "" then
                    True
                else
                    False
            Nothing ->
                False
 