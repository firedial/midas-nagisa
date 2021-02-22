module Model.Attribute exposing 
    ( AttributeElement
    , AttributeElements
    , KindElements
    , PurposeElements
    , PlaceElements
    , AttributeCollection
    , AttributesCollection
    , decodeAttributeElements
    , initAttributesCollection
    , resultAttributes
    , getAttributeNumber
    )

import Json.Decode
import Http 

type alias AttributeElements = List AttributeElement
type alias KindElements = List AttributeElement
type alias PurposeElements = List AttributeElement
type alias PlaceElements = List AttributeElement

type alias AttributeElement =
    { id : Int
    , name : String
    , description : String
    , category_id : Int
    }

type alias AttributesCollection =
    { kindCollection : AttributeCollection
    , purposeCollection : AttributeCollection
    , placeCollection : AttributeCollection
    }

type alias AttributeCollection =
    { attributeElements : AttributeElements
    }


decodeAttributeElements : Json.Decode.Decoder (List AttributeElement)
decodeAttributeElements =
    Json.Decode.list decodeAttributeElement

decodeAttributeElement : Json.Decode.Decoder AttributeElement
decodeAttributeElement =
    Json.Decode.map4 AttributeElement
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "category_id" Json.Decode.int)

initAttributesCollection : AttributesCollection
initAttributesCollection =
    AttributesCollection
        (AttributeCollection [])
        (AttributeCollection [])
        (AttributeCollection [])



resultAttributes : Result Http.Error AttributeElements -> AttributeElements
resultAttributes result =
    case result of
        Ok attributes ->
            attributes
        _ ->
            []


getAttributeNumber : AttributeElements -> Maybe String -> Maybe Int
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

getAttributeNumberExpect : AttributeElements -> Maybe String -> Maybe Int
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
        
enableAttribute : AttributeElements -> String -> AttributeElements
enableAttribute attributes str =
    List.filter (isStartStringAttribute str) attributes

isStartStringAttribute : String -> AttributeElement -> Bool
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
 