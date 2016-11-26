module State exposing (..)

import Types exposing (Message)


type alias State =
    { messages : List Message
    }


init =
    { messages = []
    }


addMessage message state =
    { state | messages = List.append state.messages [ message ] }
