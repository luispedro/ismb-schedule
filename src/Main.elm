module Main exposing (..)

import Browser
import Json.Decode as J
import Set as S

import Html
import Http
import Task
import Time
import Url.Builder
import Url
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HE

import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table

type alias Talk =
    { day : String
    , time : String
    , speaker : Maybe String
    , room : String
    , title : String
    , session : String
    , abstract : Maybe String
    }

decodeTalk : J.Decoder Talk
decodeTalk = J.map7 Talk
    (J.field "Date" J.string)
    (J.field "Timespan" J.string)
    (J.field "Speaker" (J.nullable J.string))
    (J.field "Room" J.string)
    (J.field "Title" J.string)
    (J.field "Track" J.string)
    (J.field "Abstract" (J.nullable J.string))

type SortOrder =
    ByTime
    | ByAuthor
    | BySession


type alias SimpleTime = { day : Int, hour : Int, min : Int, year : Int, month : Int }

monthToInt : Time.Month -> Int
monthToInt m = case m of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12


getSimpleTime : Time.Posix -> SimpleTime
getSimpleTime t =
    let
        hourUTC = Time.toHour Time.utc t
        minUTC = Time.toMinute Time.utc t

    in
        { day = Time.toDay Time.utc t
        , hour = hourUTC + 1 -- - 4 is to adjust to Quebec time
        , min = minUTC
        , year = Time.toYear Time.utc t
        , month = Time.toMonth Time.utc t |> monthToInt
        }

type alias FilterSet =
    { days : S.Set String
    , session : String
    , sessionFilterState : Dropdown.State
    , speaker : String
    , title : String
    , abstract : String
    , showFullAbstractsAll : Bool
    , expandAbstracts : S.Set String
    , sortOrder : SortOrder
    , now : SimpleTime
    , showPastTalks : Bool
    , talks : List Talk
    }

conferenceActive = False
baseURL = "https://ismb-schedule.luispedro.org"

initFilters : List Talk -> FilterSet
initFilters talks =
    { days = talks
                |> List.map .day
                |> S.fromList
    , session = ""
    , sessionFilterState = Dropdown.initialState
    , speaker = ""
    , title = ""
    , abstract = ""
    , showFullAbstractsAll = False
    , expandAbstracts = S.empty
    , sortOrder = ByTime
    , now = { day = 11, hour = 12, min = 0, year = 2025, month = 7}
    , showPastTalks = not conferenceActive
    , talks = talks
    }

applyUrlFilters : Url.Url -> FilterSet -> FilterSet
applyUrlFilters url fset =
    case url.query of
        Nothing -> fset
        Just q ->
            let
                pairs : Dict String String
                pairs =
                    q
                        |> String.split "&"
                        |> List.filterMap (\p ->
                            case String.split "=" p of
                                [k,v] ->
                                    Url.percentDecode v |> Maybe.withDefault v |> \val -> Just (k, val)
                                [k] -> Just (k, "")
                                _ -> Nothing
                        )
                        |> Dict.fromList
            in
            { fset
                | speaker = Dict.get "speaker" pairs |> Maybe.withDefault fset.speaker
                , title = Dict.get "title" pairs |> Maybe.withDefault fset.title
                , abstract = Dict.get "abstract" pairs |> Maybe.withDefault fset.abstract
            }

type Model =
    Loading Url.Url
    | LoadFailed String
    | ShowTalks FilterSet

main = let
        getTalks : Cmd Msg
        getTalks = Http.get
            { url = "ISMB_2025_All_sessions.json"
            , expect = Http.expectJson GotData (J.list decodeTalk)
            }
    in Browser.application
        { init = \() url _ -> (Loading url, getTalks)
        , update = update
        , subscriptions = \model -> case model of
            Loading _ -> Sub.none
            LoadFailed _ -> Sub.none
            ShowTalks m -> Sub.batch
                                [ Dropdown.subscriptions m.sessionFilterState SessionFilterChanged
                                , Time.every (15 * 60 * 1000) CurTime -- Every fifteen minutes
                                ]
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = \_ -> Ignore
        }

type Msg =
    GotData (Result Http.Error (List Talk))
    | CurTime Time.Posix
    | ToggleDayFilter String
    | SetSessionFilter String
    | SessionFilterChanged Dropdown.State
    | ToggleShowPastTalks
    | ToggleShowFullAbstract
    | ExpandAbstract String
    | SetSortOrder SortOrder
    | UpdateTitleFilter String
    | UpdateSpeakerFilter String
    | UpdateAbstractFilter String
    | UrlChanged Url.Url
    | Ignore

update msg model =
    let nmodel = case msg of
            GotData r -> case r of
              Ok d -> case model of
                    Loading url -> ShowTalks <| applyUrlFilters url (initFilters d)
                    _ -> ShowTalks <| initFilters d
              Err err -> LoadFailed <| case err of
                Http.BadUrl e -> "BadURL: " ++ e
                Http.Timeout -> "TimeOut"
                Http.NetworkError -> "NetworkError"
                Http.BadStatus c -> "BadStatus: " ++ String.fromInt c
                Http.BadBody e -> "BadBody: " ++ e
            _ -> case model of
                Loading url -> case msg of
                    UrlChanged newUrl -> Loading newUrl
                    _ -> model
                LoadFailed err -> model
                ShowTalks m -> case msg of
                    GotData _ -> model -- impossible, but needed to satisfy the compiler
                    UrlChanged newUrl -> ShowTalks <| applyUrlFilters newUrl m
                    CurTime t -> if conferenceActive
                                then ShowTalks <| adjustDays { m | now = getSimpleTime t}
                                -- conference has passed, so time is not updated anymore
                                else ShowTalks m
                    ToggleDayFilter d ->
                        let
                            newSet =
                                if S.member d m.days
                                then S.remove d m.days
                                else S.insert d m.days
                        in ShowTalks { m | days = newSet }
                    SetSessionFilter d -> ShowTalks { m | session = d }
                    SessionFilterChanged s -> ShowTalks { m | sessionFilterState = s }
                    ToggleShowFullAbstract -> ShowTalks { m | showFullAbstractsAll = not m.showFullAbstractsAll }
                    ToggleShowPastTalks -> ShowTalks <| adjustDays { m | showPastTalks = not m.showPastTalks }
                    ExpandAbstract a -> ShowTalks { m | expandAbstracts = S.insert a m.expandAbstracts }
                    SetSortOrder o -> ShowTalks { m | sortOrder = o }
                    UpdateTitleFilter t -> ShowTalks { m | title = t }
                    UpdateSpeakerFilter t -> ShowTalks { m | speaker = t }
                    UpdateAbstractFilter t ->  ShowTalks { m | abstract = t }
                    Ignore -> ShowTalks m
        cmd = case msg of
            GotData (Ok _) -> Task.perform CurTime Time.now
            _ -> Cmd.none
    in (nmodel, cmd)


add15mins : (Int, Int) -> (Int, Int)
add15mins (h, m) =
    if m > 45
    then (h + 1, m - 45)
    else (h, m + 15)

hasPassed : Talk -> SimpleTime -> Bool
hasPassed t now =
    let
        (startH15, startM15) = add15mins <| parseTime t.time
    in
        if now.year > 2025
        then True
        else if now.month < 7
        then False
        else if talkDay t.day < now.day
        then True
        else if talkDay t.day > now.day
        then False
        else startH15 < now.hour || startH15 == now.hour && startM15 < now.min

adjustDays : FilterSet -> FilterSet
adjustDays m =
    if m.showPastTalks
    then { m | days = List.map .day m.talks |> S.fromList }
    else let
            asDayString : Int -> String
            asDayString d = (String.fromInt d) ++ " July"
            ndays : S.Set String
            ndays = List.foldl (\d active -> S.remove (asDayString d) active) m.days (List.range 23 (m.now.day - 1))
        in { m | days = ndays }


talkDay : String -> Int
talkDay t = case String.split " " t of
    [n, _] -> String.left 2 n |> String.toInt |> Maybe.withDefault 0
    _ -> 0


view : Model -> Browser.Document Msg
view m =
    { title = "ISMB 2025 - Schedule"
    , body =
        [ CDN.stylesheet
        , CDN.fontAwesome
        , Html.node "link"
            [ HtmlAttr.rel "stylesheet"
            , HtmlAttr.href "style.css"
            ]
            []
        , viewModel m
        , footer
    ]}


parseTime : String -> (Int, Int)
parseTime t =
    let
        parts = t
                |> String.split "-"
                |> List.map String.trim
                |> List.head
                |> Maybe.withDefault ""
                |> String.split ":"
                |> List.map String.trim
    in
        ( parts
            |> List.head
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 0
        , parts
            |> List.tail
            |> Maybe.andThen List.head
            |> Maybe.andThen String.toInt
            |> Maybe.withDefault 0
        )

clearableInput : String -> (String -> msg) -> Html msg
clearableInput val onInput =
    Html.div [ HtmlAttr.class "input-container" ]
        [ Html.input [ HtmlAttr.type_ "text", HtmlAttr.value val, HE.onInput onInput ] []
        , Html.span [ HtmlAttr.class "clear-button", HE.onClick (onInput "") ] [ Html.text "\u{00D7}" ]
        ]

viewModel model = case model of
    Loading _ -> Html.text "Loading..."
    LoadFailed err -> Html.text ("Load error: " ++ err)
    ShowTalks m ->
        let
            filterDays = List.filter (\t -> S.member t.day m.days)
            filterSpeakers = List.filter (\t -> String.contains (String.toLower m.speaker) (String.toLower (Maybe.withDefault "" t.speaker)))
            filterTitles = List.filter (\t -> String.contains (String.toLower m.title) (String.toLower t.title))
            filterAbstracts = List.filter (\t -> String.contains (String.toLower m.abstract) (String.toLower (Maybe.withDefault "" t.abstract)))
            filterSessions = List.filter (\t -> m.session == "" || m.session == t.session)
            filterPastTalks = List.filter (\t -> m.showPastTalks || not (hasPassed t m.now))
            sel = m.talks
                    |> filterDays
                    |> filterSessions
                    |> filterTitles
                    |> filterSpeakers
                    |> filterPastTalks
                    |> filterAbstracts
                    |> (case m.sortOrder of
                        ByTime -> List.sortBy (\t -> (t.day, parseTime t.time, t.session))
                        ByAuthor -> List.sortBy (\t -> (Maybe.withDefault "ZZZ" t.speaker, t.day, parseTime t.time))
                        BySession -> List.sortBy (\t -> (t.session, t.day, parseTime t.time))
                        )
            allDays = List.map (\t -> t.day) m.talks
                        |> S.fromList
                        |> S.toList
                        |> List.sort
            allSessions = m.talks
                        |> List.map .session
                        |> S.fromList
                        |> S.toList
                        |> List.sort
        in Grid.containerFluid [HtmlAttr.id "main"]
            [ Grid.simpleRow
                [ Grid.col []
                    [ Html.h1 [] [ Html.text "ISMB 2025" ]
                    , Html.p [] [ Html.text "This is a list of all sessions at ISMB 2025."]
                    ]
                ]
            , Grid.simpleRow
                    [ Grid.col [ ]
                        (((Html.h4 [] [Html.text "Filter by days" ])::
                        List.map (\d ->
                                Button.button
                                        [ (if S.member d m.days then Button.primary else Button.outlineSecondary)
                                        , Button.onClick (ToggleDayFilter d)
                                        ]
                                        [ Html.text d ]
                            ) allDays) ++ (
                                if conferenceActive
                                then [Button.button
                                        [ (if m.showPastTalks then Button.primary else Button.outlineSecondary)
                                        , Button.onClick ToggleShowPastTalks
                                        ]
                                        [ Html.text (if m.showPastTalks then "Hide past talks" else "Show past talks") ]]
                                else []))
                    , Grid.col [ ]
                        (let
                            filter =
                                allSessions
                                        |> List.map (\s ->
                                                Dropdown.buttonItem
                                                    [ HE.onClick (SetSessionFilter s)
                                                    ]
                                                    [ Html.text s ]
                                                )
                        in [Html.h4 [] [Html.text "Filter by session" ]
                            , Dropdown.dropdown
                                m.sessionFilterState
                                { options = [ ]
                                , toggleMsg = SessionFilterChanged
                                , toggleButton =
                                    Dropdown.toggle [ ] [ Html.text "Select session" ]
                                , items =
                                    ([ Dropdown.buttonItem [ HE.onClick (SetSessionFilter "") ] [ Html.text "All sessions" ]
                                    , Dropdown.divider ] ++ filter)
                                }
                            , if m.session /= ""
                                then Html.p [] [ Html.text "Currently showing only: ", Html.i [] [ Html.text m.session ] ]
                                else Html.p [] [ ]
                            ])
                    , Grid.col [ ]
                        [ Html.h4 [] [ Html.text "Filter by speaker" ]
                        , clearableInput m.speaker UpdateSpeakerFilter
                        ]
                    , Grid.col [ ]
                        [ Html.h4 [] [ Html.text "Filter by title" ]
                        , clearableInput m.title UpdateTitleFilter
                        ]
                    , Grid.col [ ]
                        [ Html.h4 [] [ Html.text "Filter by abstract" ]
                        , clearableInput m.abstract UpdateAbstractFilter
                        ]
                    ]

            , Grid.simpleRow
                    [ Grid.col [ ]
                [ Html.p [] [ Html.text ("Showing " ++ String.fromInt (List.length sel) ++ " talks") ]
                , Button.button
                    [ (if m.showFullAbstractsAll then Button.primary else Button.outlineSecondary)
                    , Button.onClick ToggleShowFullAbstract
                    ]
                    [ Html.text (if m.showFullAbstractsAll then "Trim abstracts" else "Expand abstracts") ]
                , Table.table
                    { options = [ Table.striped, Table.hover, Table.responsive ]
                    , thead =  Table.simpleThead
                        [ Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder ByTime) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "When?" ] ]
                        , Table.th [] [ Html.text "Room" ]
                        , Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder ByAuthor) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "Speaker" ] ]
                        , Table.th [] [ Html.text "Title" ]
                        , Table.th [ Table.cellAttr <| HE.onClick (SetSortOrder BySession) ] [ Html.a [HtmlAttr.href "#" ] [ Html.text "Session" ] ]
                        , Table.th [] [ Html.text "Abstract" ]
                        ]
                    , tbody =
                        sel
                            |> List.map (\t ->
                                Table.tr []
                                    [ Table.td [] [
                                            Html.text <| t.day ++ " " ++ t.time
                                            ,Html.a [ HtmlAttr.href <| createGoogleCalLink t, HtmlAttr.target "_blank" ]
                                                [ Html.text " (add to calendar)" ]
                                            ,Html.text " "
                                            ,Html.a [ HtmlAttr.href <| createShareLink t, HtmlAttr.target "_blank" ]
                                                [ Html.i [ HtmlAttr.class "fa fa-share" ] [] ]
                                            ]
                                    , Table.td [] [ Html.text t.room ]
                                    , Table.td [] [ showHits m.speaker (Maybe.withDefault "" t.speaker) ]
                                    , Table.td [] [ showHits m.title t.title ]
                                    , Table.td [] [ Html.text t.session ]
                                    , Table.td [] ( showAbstract m t.abstract )
                                    ])
                            |> Table.tbody []
                    }
                ]
            ]
        ]

footer : Html msg
footer =
        Html.div [HtmlAttr.class "footer"]
                [ Html.p [] [ Html.text "Created by "
                            , Html.a [ HtmlAttr.href "https://luispedro.org/"]
                                [ Html.text "Luis Pedro Coelho" ]
                            , Html.text ". Code is available on "
                            , Html.a [ HtmlAttr.href "https://github.com/luispedro/ismb-schedule" ]
                                [ Html.text "GitHub" ]
                            ]
                ]


showAbstract : FilterSet -> Maybe String -> List (Html.Html Msg)
showAbstract m t = case t of
    Nothing -> [Html.text ""]
    Just abstract ->
        let
            showFull = m.showFullAbstractsAll || S.member abstract m.expandAbstracts
            maxLen = 150
            trimmed = String.left maxLen abstract
        in
        if showFull
        then [showHits m.abstract abstract]
        else [showHits m.abstract trimmed
            , Html.span [ HE.onClick (ExpandAbstract abstract) ]
                [ Html.text " [...]" ]
            ]


showHits filter abstract =
    if filter == ""
    then Html.text abstract
    else
    let
        matches : List Int
        matches = String.indexes (String.toLower filter) (String.toLower abstract)
        showMatch : Int -> Int -> Html msg
        showMatch start ix =
            let
                before = String.slice start ix abstract
            in
                Html.span []
                    [Html.text before
                    ,Html.strong [] [Html.text <| String.slice ix (ix + String.length filter) abstract]]
        showMatches : Int -> List Int -> List (Html msg)
        showMatches start ixs = case ixs of
            [] -> [Html.text <| (String.slice start (String.length abstract) abstract)]
            ix::rest -> showMatch start ix :: showMatches (ix + String.length filter) rest
    in
    Html.p []
        (showMatches 0 matches)

asCalendarTime : String -> String -> String
asCalendarTime day time =
    let
        dayn = talkDay day
        startEnd =
            String.split "-" time
            |> List.map (String.split ":")
        adjustTimezone : String -> String
        adjustTimezone =
            -- - 1 is to adjust for Montreal time
            String.toInt >> Maybe.withDefault 0 >> (\t -> t - 1) >> String.fromInt >> (\t -> if String.length t == 1 then "0" ++ t else t)
    in
    case startEnd of
        [[hourStart, minuteStart], [hourEnd, minuteEnd]] ->
            String.concat
                [ "2025"
                , "07"
                , String.fromInt dayn
                , "T"
                , adjustTimezone hourStart
                , minuteStart
                , "00"
                , "Z"
                , "/"
                , "2025"
                , "07"
                , String.fromInt dayn
                , "T"
                , adjustTimezone hourEnd
                , minuteEnd
                , "00"
                , "Z"
                ]
        _ -> ""


createGoogleCalLink : Talk -> String
createGoogleCalLink talk =
    Url.Builder.crossOrigin
        "https://calendar.google.com" ["calendar", "event"]
        [ Url.Builder.string "text" (Maybe.withDefault "" talk.speaker ++ ": " ++ talk.title)
        , Url.Builder.string "dates" (asCalendarTime talk.day talk.time)
        , Url.Builder.string "details" (String.left 200 <| Maybe.withDefault "" talk.abstract)
        , Url.Builder.string "location" talk.room
        , Url.Builder.string "action" "TEMPLATE"
        ]

createShareLink : Talk -> String
createShareLink talk =
    case talk.speaker of
        Nothing -> ""
        Just spk ->
            Url.Builder.crossOrigin
                baseURL []
                [ Url.Builder.string "speaker" spk ]
