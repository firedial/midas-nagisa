module Repository.AttributeCollection exposing (Model, init)

import Model.AttributeCollection 
import Request.Attribute

type alias Model =
    { errorMsg : String
    , attributeCollections : Model.AttributeCollection.AttributeCollections
    }

init : ( Model, Cmd msg )
init = 
    let
        ( kindModel, _ ) = Request.Attribute.init "kind"
        kindCollection = Model.AttributeCollection.AttributeCollection kindModel.attributes

        ( purposeModel, _ ) = Request.Attribute.init "purpose"
        purposeCollection = Model.AttributeCollection.AttributeCollection purposeModel.attributes

        ( placeModel, _ ) = Request.Attribute.init "place"
        placeCollection = Model.AttributeCollection.AttributeCollection placeModel.attributes

        attributeCollections = Model.AttributeCollection.AttributeCollections kindCollection purposeCollection placeCollection
    in
    ( Model "" attributeCollections, Cmd.none )

