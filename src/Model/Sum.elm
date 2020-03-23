module Model.Sum exposing (Sum, Sums)

type alias Sum =
    { id : Int
    , amount_sum : Int
    , date : String
    }

type alias Sums = List Sum
