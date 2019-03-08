port module Model exposing (EdgeData, Format(..), Gr, Model, Msg(..), NodeData, SortType(..), Source(..), Tab(..), init, subscriptions, update)

--import Form exposing (Form, Msg(Submit))

import Dict
import File.Download as Download
import Form exposing (Form, Msg(..))
import Form.Error as Error
import Form.Field as Field
import Form.Validate exposing (Validation, customValidation, emptyString, field, int, map3, map5, oneOf, string)
import Graph as G
import Http
import IntDict
import Json.Decode exposing (decodeString)
import Platform.Cmd as Cmd
import SaveLoad exposing (..)
import Search exposing (..)
import SsApi exposing (Paper, setInitialNick)
import Vis exposing (..)


type alias Gr =
    G.Graph NodeData EdgeData


type alias NodeData =
    { nid : G.NodeId
    , label : String
    , definition : String
    }


type alias EdgeData =
    { eid : EdgeId
    , from : G.NodeId
    , to : G.NodeId
    , label : String
    , definition : String
    }


type Format
    = ElmGraph
    | Dot
    | Tgf


type Msg
    = NodeFormMsg Form.Msg
    | EdgeFormMsg Form.Msg
    | SsRequest SsApi.PaperInfo
    | GotJson (Result Http.Error Paper)
    | ChangeFormat Format
    | ChangeTab Tab
    | RemoveNode
      -- Graph interaction
    | NodeSelected G.NodeId
    | EdgeSelected EdgeId
    | Unselected ()
    | FilterChanged String
    | NicknameChanged String
    | SelectChanged String
    | AddBarChanged String
    | GotFile String
    | SaveFile
    | InfluentialClick
    | SetSortType String
    | SMsg SearchMsg


type Tab
    = References
    | Citations
    | Info


type Source
    = SemanticScholar
    | ArXiv
    | DOI


type SortType
    = Alphabetical
    | Newest
    | Oldest


type alias Model =
    { graph : Gr
    , gens : IdGenerators
    , format : Format
    , tab : Tab
    , selectedNode : Maybe G.NodeId
    , selectedEdge : Maybe EdgeId
    , paperMap : IntDict.IntDict Paper
    , listFilter : String
    , selectSource : Source
    , addBar : String
    , influentialFilter : Bool
    , sortType : SortType
    , searchMod : SModel
    }


type alias IdGenerators =
    { nodeUid : Int
    , edgeUid : Int
    }


incrementNodes : Model -> Model
incrementNodes m =
    { m | gens = IdGenerators (m.gens.nodeUid + 1) m.gens.edgeUid }


incrementEdges : Int -> Model -> Model
incrementEdges n m =
    { m | gens = IdGenerators m.gens.nodeUid (m.gens.edgeUid + n) }


incrementEdgesNodes : Int -> Model -> Model
incrementEdgesNodes n m =
    { m
        | gens =
            IdGenerators (m.gens.nodeUid + 1)
                (m.gens.edgeUid + n)
    }



--firstNode = { nid=0, label="aaa", definition="aaaaaa" }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { graph = G.empty
      , gens = IdGenerators 0 0
      , format = ElmGraph
      , tab = References
      , selectedNode = Nothing
      , selectedEdge = Nothing
      , paperMap = IntDict.empty
      , listFilter = ""
      , selectSource = SemanticScholar
      , addBar = "357776cd7ee889af954f0dfdbaee71477c09ac18"
      , influentialFilter = False
      , sortType = Alphabetical
      , searchMod = initSearch
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SsRequest paperInfo ->
            if not (String.isEmpty paperInfo.ssId) then
                let
                    _ =
                        Debug.log "ss_request" paperInfo.ssId
                in
                ( model, SsApi.call paperInfo.ssId GotJson )

            else
                let
                    _ =
                        Debug.log "empty ssId" ""
                in
                addPaperToGraph
                    (setInitialNick
                        (Paper paperInfo.title paperInfo.authors paperInfo.year paperInfo.title [] [] "" Nothing Nothing Nothing)
                    )
                    model

        GotJson result ->
            case result of
                Ok s ->
                    addPaperToGraph (setInitialNick s) model

                Err e ->
                    let
                        _ =
                            Debug.log "error" e
                    in
                    ( model, Cmd.none )

        NodeSelected n ->
            ( { model | selectedNode = Just n }, Cmd.none )

        Unselected _ ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ChangeTab t ->
            ( { model | tab = t }, Cmd.none )

        RemoveNode ->
            case model.selectedNode of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    ( { model | paperMap = IntDict.remove index model.paperMap }
                    , Vis.removeNode index
                    )

        FilterChanged s ->
            ( { model | listFilter = s }, Cmd.none )

        NicknameChanged s ->
            case model.selectedNode of
                Nothing ->
                    ( model, Cmd.none )

                Just index ->
                    ( { model
                        | paperMap =
                            IntDict.update
                                index
                                (\mv ->
                                    case mv of
                                        Nothing ->
                                            Nothing

                                        Just v ->
                                            Just { v | nickname = s }
                                )
                                model.paperMap
                      }
                    , Vis.updateNode
                        { id = index
                        , label = s
                        , level =
                            case IntDict.get index model.paperMap of
                                Nothing ->
                                    2015

                                Just y ->
                                    y.year
                        }
                    )

        SelectChanged s ->
            let
                _ =
                    Debug.log s s
            in
            ( case s of
                "Semantic Scholar" ->
                    { model | selectSource = SemanticScholar }

                "ArXiv" ->
                    { model | selectSource = ArXiv }

                "DOI" ->
                    { model | selectSource = DOI }

                _ ->
                    model
            , Cmd.none
            )

        AddBarChanged s ->
            ( { model | addBar = s }, Cmd.none )

        GotFile s ->
            let
                pMap =
                    decodeString paperDictDecoder s
            in
            case pMap of
                Err e ->
                    let
                        _ =
                            Debug.log "Error while reading file: " e
                    in
                    ( model, Cmd.none )

                Ok pm ->
                    let
                        papers =
                            Debug.log "list" <| Dict.values pm
                    in
                    List.foldl
                        (\paper ->
                            \( mod, cmd ) ->
                                let
                                    ( newMod, cmd2add ) =
                                        addPaperToGraph paper mod
                                in
                                ( newMod, Cmd.batch [ cmd2add, cmd ] )
                        )
                        ( model, Cmd.none )
                        papers

        SaveFile ->
            ( model, Download.string "demo.json" "text/plain" (encodePaperMap model.paperMap) )

        InfluentialClick ->
            ( { model | influentialFilter = not model.influentialFilter }, Cmd.none )

        SetSortType s ->
            case s of
                "Alphabetical" ->
                    ( { model | sortType = Alphabetical }, Cmd.none )

                "Newest" ->
                    ( { model | sortType = Newest }, Cmd.none )

                _ ->
                    ( { model | sortType = Oldest }, Cmd.none )

        SMsg smsg ->
            let
                ( smod, scmd ) =
                    searchReducer smsg model.searchMod
            in
            ( { model | searchMod = smod }, Cmd.map SMsg scmd )

        _ ->
            ( model, Cmd.none )


addPaperToGraph : SsApi.Paper -> Model -> ( Model, Cmd Msg )
addPaperToGraph s model =
    if
        List.any
            (\paper -> s.ssId /= "" && paper.ssId == s.ssId)
            (IntDict.values model.paperMap)
    then
        ( model, Cmd.none )

    else
        let
            ( refList, citList ) =
                Debug.log "received" (SsApi.getConnection s model.paperMap)

            newId =
                model.gens.nodeUid

            n_ref =
                List.length refList

            n_cit =
                List.length citList

            firstEdgeId =
                model.gens.edgeUid
        in
        ( incrementEdgesNodes (n_ref + n_cit)
            { model
                | paperMap = IntDict.insert newId s model.paperMap
            }
        , Cmd.batch
            ([ Vis.addNode { id = model.gens.nodeUid, label = s.nickname, level = s.year } ]
                ++ Vis.addSeveralEdgesFrom newId refList firstEdgeId
                ++ Vis.addSeveralEdgesTo newId citList (firstEdgeId + n_ref)
            )
        )


port gotFile : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Vis.nodeSelected NodeSelected
        , Vis.edgeSelected EdgeSelected
        , Vis.unselected Unselected
        , gotFile GotFile
        ]



-- Graph helpers


addNode : NodeData -> Gr -> Gr
addNode ndata =
    let
        newNode =
            -- store all data as G.Node's label
            G.Node ndata.nid ndata
    in
    G.insert (G.NodeContext newNode IntDict.empty IntDict.empty)


updateNode : NodeData -> Gr -> Gr
updateNode ndata =
    let
        updatedNode =
            G.Node ndata.nid ndata
    in
    G.update ndata.nid (Maybe.map (\oldCtx -> { oldCtx | node = updatedNode }))


addEdge : EdgeData -> Gr -> Gr
addEdge ({ eid, from, to, label, definition } as edata) =
    let
        insertOutgoingEdge : G.NodeId -> EdgeData -> Maybe (G.NodeContext NodeData EdgeData) -> Maybe (G.NodeContext NodeData EdgeData)
        insertOutgoingEdge toId edgeData =
            Maybe.map
                (\oldCtx ->
                    { oldCtx
                        | outgoing = IntDict.insert toId edgeData oldCtx.outgoing
                    }
                )
    in
    G.update from (insertOutgoingEdge to edata)


lookupEdge : EdgeId -> Gr -> Maybe (G.Edge EdgeData)
lookupEdge eid gr =
    G.edges gr
        |> List.filter (\edge -> edge.label.eid == eid)
        |> List.head


removeEdge : EdgeId -> Gr -> Gr
removeEdge eid gr =
    let
        maybeEdge =
            lookupEdge eid gr

        removeOutgoingEdge : G.NodeId -> Maybe (G.NodeContext NodeData EdgeData) -> Maybe (G.NodeContext NodeData EdgeData)
        removeOutgoingEdge toId =
            Maybe.map
                (\oldCtx ->
                    { oldCtx
                        | outgoing = IntDict.remove toId oldCtx.outgoing
                    }
                )
    in
    case maybeEdge of
        Nothing ->
            gr

        Just edge ->
            G.update edge.from (removeOutgoingEdge edge.to) gr


anyString : Validation a String
anyString =
    oneOf [ emptyString, string ]
