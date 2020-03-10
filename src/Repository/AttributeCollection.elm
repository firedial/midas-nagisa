module Repository.AttributeCollection exposing (Model, Msg, init, update)

-- import Model.AttributeCollection 
import Request.Attribute

type Msg 
    = GetKind Request.Attribute.Msg
    | GetPlace Request.Attribute.Msg
    | GetPurpose Request.Attribute.Msg

type alias Model =
    { errorMsg : String
    , kindAttributeModel : Request.Attribute.Model
    , purposeAttributeModel : Request.Attribute.Model
    , placeAttributeModel : Request.Attribute.Model
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        GetKind msg_ ->
            let
                ( modelAttribute, _ ) = Request.Attribute.update msg_ model.kindAttributeModel
            in
            ( { model | kindAttributeModel = modelAttribute }, Cmd.none )
        GetPurpose msg_ ->
            let
                ( modelAttribute, _ ) = Request.Attribute.update msg_ model.purposeAttributeModel
            in
            ( { model | purposeAttributeModel = modelAttribute }, Cmd.none )
        GetPlace msg_ ->
            let
                ( modelAttribute, _ ) = Request.Attribute.update msg_ model.placeAttributeModel
            in
            ( { model | placeAttributeModel = modelAttribute }, Cmd.none )
            
init : ( Model, Cmd Msg )
init = 
    let
        ( kindModel, kindMsg ) = Request.Attribute.init "kind"
        ( purposeModel, purposeMsg ) = Request.Attribute.init "purpose"
        ( placeModel, placeMsg ) = Request.Attribute.init "place"
    in
    ( Model "" kindModel purposeModel placeModel, Cmd.batch [ Cmd.map GetKind kindMsg, Cmd.map GetPurpose purposeMsg, Cmd.map GetPlace placeMsg ] )


