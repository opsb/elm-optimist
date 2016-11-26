module Types exposing (..)


type Message
    = Message { userName : String, message : String }
    | UserJoined String
