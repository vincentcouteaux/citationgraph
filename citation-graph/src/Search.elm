module Search exposing (SModel, SearchMsg(..), initSearch, searchReducer)

import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type SearchMsg
    = BarChanged String
    | Clicked
    | JsonReceived (Result Http.Error (List SearchResult))


type alias SearchResult =
    { title : String
    , link : String
    , ssid : String
    , authors : String
    , year : Int
    }


type alias SModel =
    { searchBar : String
    , results : List SearchResult
    , waiting : Bool
    }


initSearch =
    { searchBar = ""
    , results = []
    , waiting = False
    }


searchReducer : SearchMsg -> SModel -> ( SModel, Cmd SearchMsg )
searchReducer msg smod =
    case msg of
        BarChanged s ->
            ( { smod | searchBar = s }, Cmd.none )

        Clicked ->
            if smod.searchBar /= "" then
                ( { smod | waiting = True }
                , Http.get
                    { url = "http://130.143.70.28:8080/" ++ smod.searchBar
                    , expect = Http.expectJson JsonReceived resultDecoder
                    }
                )

            else
                ( { smod | results = [], waiting = False }, Cmd.none )

        JsonReceived r ->
            let
                newmod =
                    { smod | waiting = False }
            in
            case r of
                Ok l ->
                    let
                        _ =
                            Debug.log "received" l
                    in
                    ( { newmod | results = l }, Cmd.none )

                Err e ->
                    let
                        _ =
                            Debug.log "received" e
                    in
                    ( newmod, Cmd.none )


resultDecoder : D.Decoder (List SearchResult)
resultDecoder =
    D.list
        (D.succeed SearchResult
            |> required "title" D.string
            |> required "link" D.string
            |> required "ssid" D.string
            |> required "authors" D.string
            |> required "year" D.int
        )
