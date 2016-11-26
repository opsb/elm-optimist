module Store exposing (..)

import Types exposing (..)
import Phoenix
import Phoenix.Push exposing (Push)
import Phoenix.Socket as Socket exposing (Socket)


type alias Store appMsg model =
    { optimistic : model
    , pessimistic : model
    , queue : Queue appMsg
    }


type Msg appMsg
    = FromUI appMsg
    | FromRemote appMsg
    | ConfirmRemote Int


init initialModel =
    { optimistic = initialModel
    , pessimistic = initialModel
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
    -> (OptimistStatus -> appMsg -> model -> model)
    -> (appMsg -> model -> Maybe (Push (Msg appMsg)))
    -> Msg appMsg
    -> Store appMsg model
    -> ( Store appMsg model, Cmd (Msg appMsg) )
update endpoint updateModel updateRemote optimistMsg store =
    let
        ( updatedStore, cmd ) =
            store |> applyMsg endpoint updateModel updateRemote optimistMsg
    in
        ( updatedStore
            |> applyConfirmed updateModel
            |> updateOptimisticIfAllSettled
        , cmd
        )


updateOptimisticIfAllSettled store =
    if store.queue |> isQueueEmpty then
        { store | optimistic = store.pessimistic }
    else
        store


applyConfirmed updateModel store =
    let
        queue =
            store.queue
    in
        case queue.items of
            [ Confirmed appMsg ] ->
                { store
                    | pessimistic = store.pessimistic |> updateModel Pessimistic appMsg
                    , queue = { queue | items = [] }
                }
                    |> applyConfirmed updateModel

            (Confirmed appMsg) :: appMsgs ->
                { store
                    | pessimistic = store.pessimistic |> updateModel Pessimistic appMsg
                    , queue = { queue | items = appMsgs }
                }
                    |> applyConfirmed updateModel

            _ ->
                store


applyMsg endpoint updateModel updateRemote optimistMsg store =
    case optimistMsg of
        FromUI appMsg ->
            let
                maybePush =
                    updateRemote appMsg store.optimistic

                updatedOptimistic =
                    store.optimistic |> updateModel Optimistic appMsg

                ( cmd, updatedQueue2 ) =
                    case maybePush of
                        Just push ->
                            let
                                ( updatedQueue, appMsgId ) =
                                    store.queue
                                        |> enqueuePending appMsg
                            in
                                ( push
                                    |> Phoenix.Push.onOk (always (ConfirmRemote appMsgId))
                                    |> Phoenix.push endpoint
                                , updatedQueue
                                )

                        Nothing ->
                            ( Cmd.none, store.queue |> enqueueConfirmed appMsg )
            in
                ( { store
                    | optimistic = updatedOptimistic
                    , queue = updatedQueue2
                  }
                , cmd
                )

        FromRemote appMsg ->
            let
                pessimistic =
                    store.pessimistic |> updateModel Pessimistic appMsg
            in
                ( { store
                    | pessimistic = pessimistic
                  }
                , Cmd.none
                )

        ConfirmRemote appMsgId ->
            let
                updatedQueue =
                    store.queue |> dequeue appMsgId

                updatedOptimistic =
                    if store.queue |> isQueueEmpty then
                        store.pessimistic
                    else
                        store.optimistic
            in
                ( { store
                    | optimistic = updatedOptimistic
                    , queue = updatedQueue
                  }
                , Cmd.none
                )
