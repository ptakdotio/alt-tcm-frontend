module TCM exposing (main)


import Browser

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events     as Events

import Http

import Json.Decode exposing (Decoder)
import Json.Decode as Decode

import Set exposing (Set)


main =
  Browser.document
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = always Sub.none
    }


----
--- Model
--

type Model
  = Loading
  | Loaded Explorer
  | Failed (Http.Error)

type alias Explorer =
  { filters : Filters
  , movies  : List Movie
  }

type alias Filters =
  { yearMinimum     : Maybe Int
  , yearMaximum     : Maybe Int
  , genresSelected  : Set String
  , genresAvailable : Set String
  }

type alias Movie =
  { titleId     : Int
  , title       : String
  , genres      : Set String
  , releaseYear : Int
  , description : Maybe String
  }


init : () -> (Model, Cmd Msg)
init = always (Loading, getMovies)


initExplorer : List Movie -> Explorer
initExplorer movies =
  { filters =
    { yearMinimum     = Nothing
    , yearMaximum     = Nothing
    , genresSelected  = Set.empty
    , genresAvailable = List.foldl Set.union Set.empty (List.map (.genres) movies)
    }
  , movies = movies
  }


----
--- Message
--

type Msg
  = GotMovies (Result Http.Error (List Movie))
  | UpdateFilters FilterMsg

type FilterMsg
  = SetYearMinimum (Maybe Int)
  | SetYearMaximum (Maybe Int)
  | DelGenre String
  | AddGenre String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model of

  Loading -> case msg of

    GotMovies result ->
      case result of
        Ok movies   -> (Loaded (initExplorer movies), Cmd.none)
        Err problem -> (Failed problem,               Cmd.none)

    _ -> (model, Cmd.none)

  Loaded explorer -> case msg of

    UpdateFilters change ->
      let
        filters = explorer.filters
        updated = case change of
          SetYearMinimum year -> { filters | yearMinimum = year }
          SetYearMaximum year -> { filters | yearMaximum = year }
          AddGenre genre      -> { filters | genresSelected = Set.insert genre filters.genresSelected }
          DelGenre genre      -> { filters | genresSelected = Set.remove genre filters.genresSelected }
      in
        ( Loaded { explorer | filters = updated }
        , Cmd.none
        )

    _ -> (model, Cmd.none)

  _ -> (model, Cmd.none)


----
--- View
--

view : Model -> Browser.Document Msg
view model =
  { title = "TCM Movies"
  , body  =
    [ Html.h1 [] [ Html.text "TCM Movies" ]
    , case model of
        Loading         -> Html.text "Loading..."
        Failed error    -> Html.text ("Failed to load movies: " ++ explain error)
        Loaded explorer -> viewExplorer explorer
    ]
  }


viewExplorer : Explorer -> Html Msg
viewExplorer explorer =
  Html.div [] <|
    let
      filters = Html.map UpdateFilters (viewFilters explorer.filters)
      movies  = List.filter (applyFilters explorer.filters) explorer.movies
    in
      [ filters
      , viewTable movies
      ]


viewFilters : Filters -> Html FilterMsg
viewFilters filters =
  Html.form []
    [ Html.h2 [] [ Html.text "Filters" ]
    , Html.div []
      [ Html.label
        [ Attributes.for "year-minimum" ]
        [ Html.text "Minimum Release Year" ]
      , Html.input
        [ Attributes.id "year-minimum"
        , Events.onInput (SetYearMinimum << String.toInt)
        ]
        []
      ]
    , Html.div []
      [ Html.label
        [ Attributes.for "year-maximum" ]
        [ Html.text "Maximum Release Year" ]
      , Html.input
        [ Attributes.id "year-maximum"
        , Events.onInput (SetYearMaximum << String.toInt)
        ]
        []
      ]
    , Html.div []
      [ Html.label
        [ Attributes.for "genres" ]
        [ Html.text "Filter by Genre" ]
      , Html.select
        [ Events.on "change" (Decode.map AddGenre Events.targetValue) ]
        ( [ Html.option [] [ Html.text "Choose a genre" ] ] ++
          (List.map viewGenreOption <| Set.toList filters.genresAvailable) )
      , Html.ul []
        (List.map viewGenreSelected <| Set.toList filters.genresSelected)
      ]
    ]

viewGenreOption : String -> Html FilterMsg
viewGenreOption genre =
  Html.option
    [ Attributes.value genre ]
    [ Html.text genre ]

viewGenreSelected : String -> Html FilterMsg
viewGenreSelected genre =
  Html.li []
    [ Html.a
      [ Events.onClick (DelGenre genre) ]
      [ Html.text "🞪" ]
    , Html.text " "
    , Html.text genre
    ]


viewTable : List Movie -> Html Msg
viewTable movies =
  let
    tableHeader =
      Html.tr []
        [ Html.th [ Attributes.style "width" "20%" ] [ Html.text "Title" ]
        , Html.th [ Attributes.style "width" "10%" ] [ Html.text "Release Year" ]
        , Html.th [ Attributes.style "width" "70%" ] [ Html.text "Description" ]
        ]

    linkTo movie = "https://www.tcm.com/watchtcm/titles/" ++ (String.fromInt movie.titleId)

    tableRow movie =
      Html.tr
        []
        [ Html.td []
          [ Html.a
            [ Attributes.href (linkTo movie) ]
            [ Html.text movie.title ]
          ]
        , Html.td [] [ Html.text (String.fromInt movie.releaseYear) ]
        , Html.td [] [ Html.text (Maybe.withDefault "" movie.description) ]
        ]
  in
    Html.table []
     (tableHeader :: List.map tableRow movies)


applyFilters : Filters -> Movie -> Bool
applyFilters filters movie =
  let
    matchYearMinimum =
      case filters.yearMinimum of
        Just year -> movie.releaseYear >= year
        Nothing   -> True

    matchYearMaximum =
      case filters.yearMaximum of
        Just year -> movie.releaseYear <= year
        Nothing   -> True

    matchGenres =
      (Set.isEmpty filters.genresSelected) ||
      (Set.intersect filters.genresSelected movie.genres |> Set.isEmpty |> not)
  in
    matchYearMinimum &&
    matchYearMaximum &&
    matchGenres


overlap : List a -> List a -> Bool
overlap listA listB = List.any (hasMember listA) listB

hasMember : List a -> a -> Bool
hasMember xs x = List.member x xs


explain : Http.Error -> String
explain error = case error of
  Http.BadUrl s       -> "Bad URL: " ++ s
  Http.Timeout        -> "Connection timed out"
  Http.NetworkError   -> "Network error" 
  Http.BadStatus code -> "HTTP error " ++ String.fromInt code
  Http.BadBody reason -> "Bad response: " ++ reason


----
--- API Interaction
--

getMovies : Cmd Msg
getMovies =
  Http.get
    { url    = "https://tcmws.tcm.com/tcmws/latest/250"
    , expect = Http.expectJson GotMovies decodeMovies
    }

decodeMovies : Decoder (List Movie)
decodeMovies = Decode.at ["tcm", "titles"] (Decode.list decodeMovie)

decodeMovie : Decoder Movie
decodeMovie =
  Decode.map5 Movie
    (Decode.field "titleId"     (Decode.int))
    (Decode.field "name"        (Decode.string))
    (Decode.field "tvGenresArr" (Decode.map Set.fromList (Decode.list Decode.string)))
    (Decode.field "releaseYear" (Decode.int))
    (Decode.field "description" (Decode.nullable Decode.string))

