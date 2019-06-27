module SsApi exposing (Paper, PaperInfo, call, catSet, findInMapList, findMap, getConnection, isIn, last_el, paperDecoder, setInitialNick)

--import Model

import Http
import IntDict
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Set


type alias PaperInfo =
    { title : String
    , authors : List String
    , year : Int
    , ssId : String
    , isInfluential : Bool
    }


type alias Paper =
    { title : String
    , authors : List String
    , year : Int
    , ssId : String
    , references : List PaperInfo
    , citations : List PaperInfo
    , nickname : String
    , url : Maybe String
    , arxiv : Maybe String
    , doi : Maybe String
    , notes : String
    }



--, abstract : String
--, arxivId: Maybe String
--, doi: Maybe String
--}



--call : String -> Msg -> Cmd Msg


call s msg =
    Http.get
        { url =
            "https://api.semanticscholar.org/v1/paper/"
                ++ s
                ++ "?include_unknown_references=true"

        --, expect = Http.expectJson msg (D.field "title" D.string)
        , expect = Http.expectJson msg paperDecoder
        }


paperDecoder : D.Decoder Paper
paperDecoder =
    D.succeed Paper
        |> required "title" D.string
        |> required "authors" (D.list (D.field "name" D.string))
        |> required "year" D.int
        |> required "paperId" D.string
        |> required "references"
            (D.list
                (D.succeed PaperInfo
                    |> required "title" D.string
                    |> required "authors" (D.list (D.field "name" D.string))
                    |> optional "year" D.int 2015
                    |> required "paperId" D.string
                    |> required "isInfluential" D.bool
                )
            )
        |> required "citations"
            (D.list
                (D.succeed PaperInfo
                    |> required "title" D.string
                    |> required "authors" (D.list (D.field "name" D.string))
                    |> optional "year" D.int 2015
                    |> required "paperId" D.string
                    |> required "isInfluential" D.bool
                )
            )
        |> hardcoded "no_nick"
        |> optional "url" (D.map Just D.string) Nothing
        |> optional "arxivId" (D.map Just D.string) Nothing
        |> optional "doi" (D.map Just D.string) Nothing
        |> hardcoded ""


setInitialNick : Paper -> Paper
setInitialNick p =
    let
        firstWord =
            case String.split " " p.title of
                [] ->
                    ""

                h :: t ->
                    String.left 5 h

        firstAuthor =
            case p.authors of
                [] ->
                    ""

                h :: t ->
                    case last_el (String.split " " h) of
                        Nothing ->
                            ""

                        Just s ->
                            String.left 5 s

        year =
            String.right 2 (String.fromInt p.year)
    in
    { p | nickname = firstAuthor ++ year ++ firstWord }


last_el : List a -> Maybe a
last_el l =
    case l of
        [] ->
            Nothing

        h :: [] ->
            Just h

        h :: t ->
            last_el t


getConnection : Paper -> IntDict.IntDict Paper -> ( List Int, List Int )
getConnection paper dic =
    let
        refList =
            List.map (\r -> r.ssId) paper.references
    in
    let
        citList =
            List.map (\c -> c.ssId) paper.citations
    in
    let
        mapSsid =
            IntDict.map (\i -> \p -> p.ssId) dic
    in
    let
        mapRefSsid =
            IntDict.map
                (\i -> \p -> List.map (\pi -> pi.ssId) p.references)
                dic
    in
    let
        mapCitSsid =
            IntDict.map
                (\i -> \p -> List.map (\pi -> pi.ssId) p.citations)
                dic
    in
    ( catSet
        (List.filterMap
            (\s -> findMap s (IntDict.toList mapSsid))
            refList
        )
        (findInMapList (IntDict.toList mapCitSsid) paper.ssId)
    , catSet
        (List.filterMap
            (\s -> findMap s (IntDict.toList mapSsid))
            citList
        )
        (findInMapList (IntDict.toList mapRefSsid) paper.ssId)
    )


findMap : String -> List ( Int, String ) -> Maybe Int
findMap str l =
    case l of
        ( i, s ) :: t ->
            if s == str then
                Just i

            else
                findMap str t

        [] ->
            Nothing


isIn : a -> List a -> Bool
isIn x l =
    List.foldl
        (\e ->
            \b ->
                if x == e then
                    True

                else
                    b
        )
        False
        l


findInMapList : List ( Int, List String ) -> String -> List Int
findInMapList lm s =
    List.filterMap
        (\( i, l ) ->
            if isIn s l then
                Just i

            else
                Nothing
        )
        lm


catSet : List comparable -> List comparable -> List comparable
catSet l1 l2 =
    Set.toList (Set.fromList (l1 ++ l2))
