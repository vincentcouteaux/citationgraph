module View exposing (view)

import Form exposing (Form)
import Form.Input exposing (..)
import GraphFormatter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as E
import Http
import IntDict
import Json.Decode as D
import Model exposing (..)
import SaveLoad exposing (encodePaperMap)
import Search
import SsApi exposing (PaperInfo)


view : Model -> Html Msg
view m =
    div []
        [ leftPanel m
        , visContainer
        , rightPanel m
        ]


visContainer : Html Msg
visContainer =
    div [ class "fullscreen", id "vis-container" ]
        []


selectedTabs : Model -> Html Msg
selectedTabs m =
    div [ class "upbuttons" ]
        [ Html.button
            [ E.onClick (Model.ChangeTab References)
            , disabled (m.tab == References)
            ]
            [ Html.text "References" ]
        , Html.button
            [ E.onClick (Model.ChangeTab Citations)
            , disabled (m.tab == Citations)
            ]
            [ Html.text "Citations" ]
        , Html.button
            [ E.onClick (Model.ChangeTab Info)
            , disabled (m.tab == Info)
            ]
            [ Html.text "Info" ]
        ]


referenceList : Model -> Html Msg
referenceList m =
    div [ style "overflow-y" "scroll" ]
        --style "display" "flex", style "flex-flow" "column" ]
        [ div [ class "upbuttons" ]
            [ input
                [ type_ "text"
                , placeholder "Search"
                , value m.listFilter
                , E.onInput FilterChanged
                ]
                []
            , div []
                [ input
                    [ type_ "checkbox"
                    , id "influential"
                    , E.onClick InfluentialClick
                    , checked m.influentialFilter
                    ]
                    []
                , label [ for "influential" ] [ text "Influential papers only" ]
                ]
            , select [ E.onInput SetSortType ]
                [ option [ selected <| m.sortType == Alphabetical ] [ text "Alphabetical" ]
                , option [ selected <| m.sortType == Newest ] [ text "Newest" ]
                , option [ selected <| m.sortType == Oldest ] [ text "Oldest" ]
                ]
            ]
        , div [ class "paperlist" ]
            (case m.selectedNode of
                Just n ->
                    let
                        mpaper =
                            IntDict.get n m.paperMap
                    in
                    case mpaper of
                        Just paper ->
                            List.map
                                paperInfoDiv
                                (List.sortWith
                                    (case m.sortType of
                                        Alphabetical ->
                                            \p1 -> \p2 -> compare p1.title p2.title

                                        Newest ->
                                            \p1 -> \p2 -> compare p2.year p1.year

                                        Oldest ->
                                            \p1 -> \p2 -> compare p1.year p2.year
                                    )
                                    (List.filter
                                        (if m.influentialFilter then
                                            .isInfluential

                                         else
                                            \_ -> True
                                        )
                                        (List.filter (filterPapers m.listFilter)
                                            (if m.tab == References then
                                                paper.references

                                             else
                                                paper.citations
                                            )
                                        )
                                    )
                                )

                        Nothing ->
                            []

                Nothing ->
                    []
            )
        ]


paperInfoDiv : PaperInfo -> Html Msg
paperInfoDiv pi =
    div [ class "paperInfoDiv" ]
        [ h4 []
            [ if pi.ssId /= "" then
                a [ href ("https://api.semanticscholar.org/" ++ pi.ssId), target "blank_" ]
                    [ text pi.title ]

              else
                text pi.title
            ]
        , h5 [] [ text (String.join " ; " pi.authors) ]
        , text (String.fromInt pi.year)
        , button [ E.onClick (Model.SsRequest pi), style "float" "right" ] [ text "Add" ]
        , div [ style "clear" "both" ] []
        ]


rightPanel : Model -> Html Msg
rightPanel m =
    div
        [ style "position" "absolute"
        , style "right" "10px"
        , style "width" "20%"
        , class "column"
        ]
        (selectedTabs m
            :: [ if m.tab == Info then
                    infoPanel m

                 else
                    referenceList m
               ]
        )


infoPanel : Model -> Html Msg
infoPanel m =
    case m.selectedNode of
        Nothing ->
            div [] []

        Just selectedNode ->
            let
                mSelectedPaper =
                    IntDict.get selectedNode m.paperMap
            in
            div []
                (case mSelectedPaper of
                    Nothing ->
                        []

                    Just selectedPaper ->
                        h1 [] [ text selectedPaper.title ]
                            :: [ h3 [] [ text (String.join " ; " selectedPaper.authors) ]
                               , h3 [] [ text (String.fromInt selectedPaper.year) ]
                               , Html.button [ E.onClick Model.RemoveNode ] [ text "Remove" ]
                               , br [] []
                               , input
                                    [ type_ "text"
                                    , value selectedPaper.nickname
                                    , E.onInput NicknameChanged
                                    ]
                                    []
                               , br [] []
                               , case selectedPaper.url of
                                    Nothing ->
                                        text ""

                                    Just url ->
                                        a [ target "_blank", rel "noopener noreferrer", href url ] [ text "Semantic Scholar link" ]
                               , br [] []
                               , case selectedPaper.arxiv of
                                    Nothing ->
                                        text ""

                                    Just id ->
                                        a [ target "_blank", rel "noopener noreferrer", href ("https://arxiv.org/pdf/" ++ id ++ ".pdf") ]
                                            [ text "Arxiv PDF" ]
                               , br [] []
                               , case selectedPaper.doi of
                                    Nothing ->
                                        text ""

                                    Just doi ->
                                        text doi
                               ]
                )


filterPapers : String -> PaperInfo -> Bool
filterPapers filter pi =
    let
        words =
            String.split " " filter

        lowcaseTitle =
            String.toLower pi.title
    in
    let
        filterList wl =
            case wl of
                [] ->
                    False

                h :: t ->
                    if String.contains (String.toLower h) lowcaseTitle then
                        True

                    else
                        filterList t
    in
    filterList words


leftPanel : Model -> Html Msg
leftPanel m =
    div [ class "column", style "position" "absolute", style "left" "10px", style "width" "20%" ]
        [ div [ class "upbuttons" ]
            [ select [ E.onInput Model.SelectChanged ]
                [ option [] [ text "Semantic Scholar" ]
                , option [] [ text "ArXiv" ]
                , option [] [ text "DOI" ]
                ]
            , input [ type_ "text", value m.addBar, E.onInput Model.AddBarChanged ] []
            , button [ E.onClick (onAdd m) ] [ text "Add" ]
            , br [] []

            --, a [ href <| "data:text/plain;charset=utf-8," ++ (encodePaperMap m.paperMap)
            --    , download "demo.txt" ]
            --    [ text "Save" ]
            , button [ E.onClick SaveFile ] [ text "Save" ]
            , br [] []
            , input [ type_ "file", id "files" ] []
            , br [] []
            , div []
                [ input
                    [ type_ "text"
                    , value m.searchMod.searchBar
                    , E.onInput (SMsg << Search.BarChanged)
                    ]
                    []
                , button
                    [ E.onClick (SMsg Search.Clicked)
                    , disabled m.searchMod.waiting
                    ]
                    [ text
                        (if m.searchMod.waiting then
                            "loading..."

                         else
                            "Search"
                        )
                    ]
                ]
            ]
        , div [ class "paperlist" ]
            (List.map
                (\res ->
                    div [ class "paperInfoDiv" ]
                        [ a
                            [ target "_blank"
                            , rel "noopener noreferrer"
                            , href res.link
                            ]
                            [ text res.title ]
                        , div [] [ text res.authors ]
                        , text (String.fromInt res.year)
                        , button
                            [ E.onClick (Model.SsRequest (PaperInfo "" [] 0 res.ssid False))
                            , style "float" "right"
                            ]
                            [ text "Add" ]
                        , div [ style "clear" "both" ] []
                        ]
                )
                m.searchMod.results
            )
        ]


onAdd : Model -> Msg
onAdd m =
    SsRequest
        (PaperInfo ""
            []
            0
            (case m.selectSource of
                Model.ArXiv ->
                    "arXiv:" ++ m.addBar

                _ ->
                    m.addBar
            )
            False
        )
