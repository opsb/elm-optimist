module Store exposing (..)

import Types exposing (..)


type alias Store model =
    { optimistic : model
    , pessimistic : model
    }


init initialModel =
    { optimistic = initialModel
    , pessimistic = initialModel
    }


update updateModel msg store =
    { store | optimistic = store.optimistic |> updateModel msg }
