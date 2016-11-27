module Optimist exposing (..)

import Types exposing (..)
import Phoenix
import Phoenix.Push exposing (Push)
import Phoenix.Socket as Socket exposing (Socket)


type alias Optimist appMsg appModel =
    { optimistic : appModel
    , pessimistic : appModel
    , queue : Queue appMsg
    }


type Msg appMsg
    = FromUI appMsg
    | FromRemote appMsg
    | ConfirmRemote Int


init initialAppModel =
    { optimistic = initialAppModel
    , pessimistic = initialAppModel
    , queue = initQueue
    }


type alias Queue appMsg =
    { uid : Int
    , items : List (QueueItem appMsg)
    }


type QueueItem appMsg
    = Pending Int
    | Confirmed appMsg


type OptimistStatus
    = Optimistic
    | Pessimistic


initQueue =
    { uid = 0
    , items = []
    }


enqueueConfirmed appMsg queue =
    { queue | items = queue.items ++ [ Confirmed appMsg ] }


enqueuePending appMsg queue =
    let
        appMsgId =
            queue.uid
    in
        ( { queue | items = queue.items ++ [ Pending appMsgId ], uid = appMsgId + 1 }
        , appMsgId
        )


dequeue appMsgId queue =
    let
        msgMatches item =
            case item of
                Pending id ->
                    (Debug.log "checking" id) == (Debug.log "appMsgId" appMsgId)

                _ ->
                    False

        updatedItems =
            queue.items |> List.filter (msgMatches >> not)
    in
        { queue | items = updatedItems }


isQueueEmpty queue =
    queue.items |> List.isEmpty


update :
    String
    -> (OptimistStatus -> appMsg -> appModel -> appModel)
    -> (appMsg -> appModel -> Maybe (Push (Msg appMsg)))
    -> Msg appMsg
    -> Optimist appMsg appModel
    -> ( Optimist appMsg appModel, Cmd (Msg appMsg) )
update endpoint updateModel updateRemote optimistMsg appModel =
    let
        ( updatedOptimist, cmd ) =
            appModel |> applyMsg endpoint updateModel updateRemote optimistMsg
    in
        ( updatedOptimist
            |> applyConfirmed updateModel
            |> updateOptimisticIfAllSettled
        , cmd
        )


updateOptimisticIfAllSettled optimist =
    if optimist.queue |> isQueueEmpty then
        { optimist | optimistic = optimist.pessimistic }
    else
        optimist


applyConfirmed updateModel optimist =
    let
        queue =
            optimist.queue
    in
        case queue.items of
            [ Confirmed appMsg ] ->
                { optimist
                    | pessimistic = optimist.pessimistic |> updateModel Pessimistic appMsg
                    , queue = { queue | items = [] }
                }
                    |> applyConfirmed updateModel

            (Confirmed appMsg) :: appMsgs ->
                { optimist
                    | pessimistic = optimist.pessimistic |> updateModel Pessimistic appMsg
                    , queue = { queue | items = appMsgs }
                }
                    |> applyConfirmed updateModel

            _ ->
                optimist


applyMsg endpoint updateModel updateRemote optimistMsg optimist =
    case optimistMsg of
        FromUI appMsg ->
            let
                maybePush =
                    updateRemote appMsg optimist.optimistic

                updatedOptimistic =
                    optimist.optimistic |> updateModel Optimistic appMsg

                ( cmd, updatedQueue2 ) =
                    case maybePush of
                        Just push ->
                            let
                                ( updatedQueue, appMsgId ) =
                                    optimist.queue
                                        |> enqueuePending appMsg
                            in
                                ( push
                                    |> Phoenix.Push.onOk (always (ConfirmRemote appMsgId))
                                    |> Phoenix.push endpoint
                                , updatedQueue
                                )

                        Nothing ->
                            ( Cmd.none, optimist.queue |> enqueueConfirmed appMsg )
            in
                ( { optimist
                    | optimistic = updatedOptimistic
                    , queue = updatedQueue2
                  }
                , cmd
                )

        FromRemote appMsg ->
            let
                pessimistic =
                    optimist.pessimistic |> updateModel Pessimistic appMsg
            in
                ( { optimist
                    | pessimistic = pessimistic
                  }
                , Cmd.none
                )

        ConfirmRemote appMsgId ->
            let
                updatedQueue =
                    optimist.queue |> dequeue appMsgId

                updatedOptimistic =
                    if optimist.queue |> isQueueEmpty then
                        optimist.pessimistic
                    else
                        optimist.optimistic
            in
                ( { optimist
                    | optimistic = updatedOptimistic
                    , queue = updatedQueue
                  }
                , Cmd.none
                )
