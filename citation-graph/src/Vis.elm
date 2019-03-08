port module Vis exposing
    ( EdgeId
    , VisEdge
    , addEdge
    , addNode
    , addSeveralEdgesFrom
    , addSeveralEdgesTo
    , edgeSelected
    , nodeSelected
    , removeEdge
    , removeNode
    , unselectAll
    , unselected
    , updateEdge
    , updateNode
    )

import Graph



-- Graph library has concept of NodeId, but not EdgeId


type alias EdgeId =
    Int


type alias VisNode =
    { id : Int
    , label : String
    , level : Int
    }


type alias VisEdge =
    { id : Int
    , from : Int
    , to : Int
    , label : String
    }


port addNode : VisNode -> Cmd msg


port updateNode : VisNode -> Cmd msg


port removeNode : Graph.NodeId -> Cmd msg


port addEdge : VisEdge -> Cmd msg


port updateEdge : VisEdge -> Cmd msg


port removeEdge : EdgeId -> Cmd msg


port nodeSelected : (Graph.NodeId -> msg) -> Sub msg


port edgeSelected : (Int -> msg) -> Sub msg


port unselected : (() -> msg) -> Sub msg


port unselectAll_ : () -> Cmd msg



-- () because ports MUST have at least one input arg
-- declare helper to avoid having to call it with ()


unselectAll : Cmd msg
unselectAll =
    unselectAll_ ()


addSeveralEdgesFrom : Int -> List Int -> Int -> List (Cmd msg)
addSeveralEdgesFrom from lTo id =
    case lTo of
        [] ->
            []

        h :: t ->
            addEdge (VisEdge id from h "") :: addSeveralEdgesFrom from t (id + 1)


addSeveralEdgesTo : Int -> List Int -> Int -> List (Cmd msg)
addSeveralEdgesTo to lFrom id =
    case lFrom of
        [] ->
            []

        h :: t ->
            addEdge (VisEdge id h to "") :: addSeveralEdgesTo to t (id + 1)
