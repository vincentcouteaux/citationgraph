module SaveLoad exposing (..)

import Json.Encode as J
import Json.Decode as D
--import Model exposing (..)
import IntDict exposing (IntDict)
import SsApi exposing (Paper, PaperInfo)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Dict exposing (Dict)

encodePaperInfo : PaperInfo -> J.Value
encodePaperInfo pi =
    J.object
        [ ("title", J.string pi.title)
        , ("authors", J.list J.string pi.authors)
        , ("year", J.int pi.year)
        , ("ssId", J.string pi.ssId)
        , ("isInfluential", J.bool pi.isInfluential )
        ]

maybe : (a -> J.Value) -> Maybe a -> J.Value
maybe typ ma =
    case ma of
        Nothing -> J.null
        Just a -> typ a

encodePaper : Paper -> J.Value
encodePaper p =
    J.object
        [ ("title", J.string p.title)
        , ("authors", J.list J.string p.authors)
        , ("year", J.int p.year)
        , ("ssId", J.string p.ssId)
        , ("references", J.list encodePaperInfo p.references)
        , ("citations", J.list encodePaperInfo p.citations)
        , ("nickname", J.string p.nickname)
        , ("url", maybe J.string p.url)
        , ("arxiv", maybe J.string p.arxiv)
        , ("doi", maybe J.string p.doi)
        ]

encodePaperMap : IntDict Paper -> String
encodePaperMap pm =
    J.encode 0 
        <| J.object (List.map (\(i, v) -> (String.fromInt i, encodePaper v)) (IntDict.toList pm))

paperDecoder : D.Decoder Paper
paperDecoder = 
    D.succeed Paper
        |> required "title" D.string
        |> required "authors" (D.list D.string)
        |> required "year" D.int
        |> required "ssId" D.string
        |> required "references" 
            (D.list
                (D.succeed PaperInfo
                    |> required "title" D.string
                    |> required "authors" (D.list D.string)
                    |> optional "year" D.int 2015
                    |> required "ssId" D.string
                    |> optional "isInfluential" D.bool False
                )
            )
        |> required "citations" 
            (D.list
                (D.succeed PaperInfo
                    |> required "title" D.string
                    |> required "authors" (D.list D.string)
                    |> optional "year" D.int 2015
                    |> required "ssId" D.string
                    |> optional "isInfluential" D.bool False
                )
            )
        |> required "nickname" D.string
        |> optional "url" (D.map Just D.string) Nothing
        |> optional "arxiv" (D.map Just D.string) Nothing
        |> optional "doi" (D.map Just D.string) Nothing

paperDictDecoder : D.Decoder (Dict String Paper)
paperDictDecoder = D.dict paperDecoder 
