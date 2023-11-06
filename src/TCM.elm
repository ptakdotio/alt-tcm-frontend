module TCM exposing (main)


import Browser

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events     as Events

import Http

import Json.Decode exposing (Decoder)
import Json.Decode as Decode


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
  | Loaded (List Movie)
  | Failed (Http.Error)

type alias Movie =
  { title       : String
  , genres      : List String
  , releaseYear : Int
  , description : Maybe String
  }


init : () -> (Model, Cmd Msg)
init = always (Loading, getMovies)


----
--- Message
--

type Msg
  = GotMovies (Result Http.Error (List Movie))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotMovies result ->
      case result of
        Ok movies   -> (Loaded movies,  Cmd.none)
        Err problem -> (Failed problem, Cmd.none)


----
--- View
--

view : Model -> Browser.Document Msg
view model =
  { title = "TCM Movies"
  , body  = List.singleton <|
      case model of
        Loading       -> Html.text "Loading..."
        Failed error  -> Html.text ("Failed to load movies: " ++ explain error)
        Loaded movies -> viewMovies movies
  }


viewMovies : List Movie -> Html Msg
viewMovies movies =
  let
    tableHeader =
      Html.tr
        []
        [ Html.th [] [ Html.text "Title" ]
        , Html.th [] [ Html.text "Release Year" ]
        , Html.th [] [ Html.text "Description" ]
        ]
  in
    Html.table [] (tableHeader :: List.map viewMovie movies)


viewMovie : Movie -> Html Msg
viewMovie movie =
  Html.tr
    []
    [ Html.td [] [ Html.text (movie.title) ]
    , Html.td [] [ Html.text (String.fromInt movie.releaseYear) ]
    , Html.td [] [ Html.text (Maybe.withDefault "" movie.description) ]
    ]



explain : Http.Error -> String
explain error = case error of
  Http.BadUrl s       -> ("Bad URL: " ++ s)
  Http.Timeout        -> "Connection timed out"
  Http.NetworkError   -> "Network error" 
  Http.BadStatus code -> ("Error " ++ String.fromInt code)
  Http.BadBody reason -> ("Bad response: " ++ reason)


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
decodeMovies =
  Decode.at ["tcm", "titles"] (Decode.list decodeMovie)

decodeMovie : Decoder Movie
decodeMovie =
  Decode.map4 Movie
    (Decode.field "name"        (Decode.string))
    (Decode.field "tvGenresArr" (Decode.list Decode.string))
    (Decode.field "releaseYear" (Decode.int))
    (Decode.field "description" (Decode.nullable Decode.string))

