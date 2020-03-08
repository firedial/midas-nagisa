module Model.AttributeCollection exposing (AttributeCollection, AttributeCollections)

import Model.Attribute

type alias AttributeCollections =
    { kindCollection : AttributeCollection
    , purposeCollection : AttributeCollection
    , placeCollection : AttributeCollection
    }

type alias AttributeCollection =
    { attribute : Model.Attribute.Attributes
    }
