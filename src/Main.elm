module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Http exposing (Error(BadUrl, NetworkError, Timeout, BadStatus, BadPayload), send, get)
import Task
import String exposing (join, padLeft)
import Time exposing (every, second)
import RemoteData exposing (..)


type alias Model =
    { times : WebData (List Int)
    }


type Msg
    = ResultReceived (WebData (List Int))
    | Refresh


init : ( Model, Cmd Msg )
init =
    ( Model Loading, getArrivalTime )


pad =
    toString >> padLeft 2 '0'


timeToBus t =
    let
        m =
            t // 60 |> pad

        s =
            rem t 60 |> pad
    in
        div
            [ class "minutes" ]
            [ text (m ++ "m:" ++ s ++ "s") ]


view : Model -> Html Msg
view model =
    let
        ( txt, data, error ) =
            case model.times of
                NotAsked ->
                    ( "Refresh", [], "" )

                Loading ->
                    ( "Loading", [], "" )

                Failure err ->
                    ( "Refresh", [], parseError err )

                Success t ->
                    ( "Refresh", t, "" )
    in
        div [ class "container" ]
            [ div []
                (List.map timeToBus data)
            , div
                [ class "refresh"
                , disabled (model.times == Loading)
                ]
                [ button [ onClick Refresh ]
                    [ text txt
                    ]
                ]
            , div
                [ class "error" ]
                [ text error ]
            ]


parseError err =
    case err of
        Timeout ->
            "The service timed out"

        NetworkError ->
            "There was some sort of network error"

        BadUrl _ ->
            "There was a bad url error"

        BadStatus _ ->
            "Bad Status Error"

        BadPayload _ _ ->
            "Bad payload error"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultReceived result ->
            ( { model | times = result }, Cmd.none )

        Refresh ->
            ( { model | times = Loading }, getArrivalTime )


sortTimes data =
    RemoteData.map List.sort data


getArrivalTime : Cmd Msg
getArrivalTime =
    send (ResultReceived << sortTimes << fromResult) <|
        get "https://api.tfl.gov.uk/StopPoint/490010849S/Arrivals?app_id=da7d2b38&app_key=6e41a466bf61c85ce50712c5622e12d1" timeDecoder


timeDecoder : Json.Decoder (List Int)
timeDecoder =
    Json.list (field "timeToStation" Json.int)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }
