module Main exposing (..)

import Html
import Http exposing (get, send)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Events exposing (onInput, onClick)
import Html.Styled.Attributes exposing (type_, placeholder, css, align, class, src)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import Debug

--import Html.Events exposing ( onInput )
--- MODEL


type alias VideosResult =
    { videoId : String
    , title : String
    , description : String
    }


type alias VideosList =
    List VideosResult


type alias Model =
    { searchInput : String
    , searchTerm : String
    , videosList : VideosList
    }


initModel : Model
initModel =
    { searchInput = ""
    , searchTerm = "Surfing boards"
    , videosList = []
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, (getVideos initModel.searchTerm) )


videoItemDecoder : Decoder VideosResult
videoItemDecoder =
    decode VideosResult
        |> requiredAt [ "id", "videoId" ] string
        |> requiredAt [ "snippet", "title" ] string
        |> requiredAt [ "snippet", "description" ] string


videoListDecoder : Decoder VideosList
videoListDecoder =
    (Json.Decode.list videoItemDecoder)


jsonDecoder : Decoder VideosList
jsonDecoder =
    (field "items" videoListDecoder)


getVideos : String -> Cmd Msg
getVideos term =
    let
        vterm = Debug.log "Searching for vterm " term
        url =
            "https://www.googleapis.com/youtube/v3/search"

        key =
            "&key=AIzaSyBOBPIU3YCv07xcQ_SQreje9hk-07SuEiw"

        search_url =
            url ++ "?part=snippet&q=" ++ term ++ key

        request =
            Http.get search_url jsonDecoder
    in
        Http.send Videos request



--- UPDATE


type Msg
    = UserClick
    | Input String
    | Videos (Result Http.Error VideosList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input val ->
            ( { model | searchInput = val }, Cmd.none )

        UserClick ->
            ( { model | searchTerm = model.searchInput }
            , (getVideos model.searchInput)
            )

        Videos (Ok value) ->
            let _ = Debug.log "values are : " value
            in
            ( { model | videosList = value }, Cmd.none )

        Videos (Err err) ->
            ( model, Cmd.none )



--- VIEW


getUrl : Maybe VideosResult -> String
getUrl video =
    case video of
        Just video ->
            "https://www.youtube.com/embed/" ++ video.videoId

        Nothing ->
            ""

displayVideosList : List VideosResult -> List (Html Msg)
displayVideosList videos =
    (List.map toHtmlLi videos)

toHtmlLi: VideosResult -> Html Msg
toHtmlLi videosResult =
    li [class "list-group-item"] [text videosResult.title]

view : Model -> Html Msg
view model =
    div []
        [ div
            []
            [ input
                [ css
                    ([ margin (px 20)
                     , textAlign center
                     , width (pct 75)
                     , fontSize (px 14)
                     ]
                    )
                , type_
                    "text"
                , placeholder "Enter the search term here..."
                , onInput Input
                ]
                []
            , button
                [ css ([ margin (px 20) ])
                , type_ "button"
                , onClick UserClick
                ]
                [ text "Search" ]
            ]
        , div [ class "col-md-8"
              , css ([ marginTop (px 10)
                       , padding (px 10)
                       ,border3 (px 1) solid (hex "ddd")
                       ,borderRadius (px 4)])]
            [ div [ class "embed-responsive embed-responsive-16by9" ]
                [ iframe
                    [ class "embed-responsive-item"
                    , css
                        ([ width (px 720)
                         , height (px 500)
                         ]
                        )
                    , src (getUrl (List.head model.videosList))
                    ]
                    []
                ]
            ]
            , ul [class "col-md-4 list-group"]
                (displayVideosList model.videosList)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }
