module Main exposing (..)

import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)
-- MODEL

type alias Model =
    VideosList

type alias VideosResult =
    {
     title : String
    , description : String

    }

type alias VideosList = List VideosResult

initModel : Model
initModel =
    []


init : ( Model, Cmd Msg )
init =
    ( initModel, getVideos )

videoItemDecoder : Decoder VideosResult
videoItemDecoder =
    decode VideosResult
        |> required "title" string
        |> required "description" string
        |> at ["snippet"]

videoListDecoder : Decoder VideosList
videoListDecoder = (list videoItemDecoder)

jsonDecoder : Decoder VideosList
jsonDecoder = (field "items" videoListDecoder)

getVideos :  Cmd Msg
getVideos =
    let
        url =
            "https://www.googleapis.com/youtube/v3/search"
        key = "&key=AIzaSyBOBPIU3YCv07xcQ_SQreje9hk-07SuEiw"

        search_url =
            url ++ "?part=snippet&q=surfing"  ++ key

        request = Http.get search_url jsonDecoder
    in
        Http.send Videos request

-- UPDATE


type Msg = Videos (Result Http.Error VideosList)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Videos (Ok value) ->
            ( value , Cmd.none )

        Videos (Err err) ->
            ( [], Cmd.none )


-- VIEW
toHtmlList : VideosList -> Html msg
toHtmlList videosList =
  ul [] (List.map toLi videosList)

toLi : VideosResult  -> Html msg
toLi s =
  li [] [ text (s.title ++ "-------------" ++ s.description) ]

view : Model -> Html Msg
view model =
    div [] [ toHtmlList model ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
