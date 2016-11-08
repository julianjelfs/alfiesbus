module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Http exposing (get)
import Task
import String exposing (join, padLeft)


type alias Model =
    { times : List Int }


type Msg
    = ResultSuccess (List Int)
    | ResultFailed Http.Error
    | Refresh


init : ( Model, Cmd Msg )
init =
    ( Model [], getArrivalTime )


secondsToMinutes t =
    let
        m =
            t // 60 |> toString |> padLeft 2 '0'

        s =
            rem t 60 |> toString |> padLeft 2 '0'
    in
        m ++ "m:" ++ s ++ "s"


view : Model -> Html Msg
view model =
    let
        ts =
            model.times
                |> List.map secondsToMinutes
    in
        div [ class "container" ]
            [ div []
                (List.map
                    (\t ->
                        div [ class "minutes" ] [ text t ]
                    )
                    ts
                )
            , div [ class "refresh" ]
                [ button [ onClick Refresh ]
                    [ text "Refresh" ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultSuccess times ->
            let
                ts =
                    times
                        |> List.sort
            in
                ( { model | times = ts }, Cmd.none )

        ResultFailed err ->
            ( Model [], Cmd.none )

        Refresh ->
            ( model, getArrivalTime )


getArrivalTime : Cmd Msg
getArrivalTime =
    (get timeDecoder "https://api.tfl.gov.uk/StopPoint/490010849S/Arrivals?app_id=da7d2b38&app_key=6e41a466bf61c85ce50712c5622e12d1")
        |> Task.perform ResultFailed ResultSuccess


timeDecoder : Json.Decoder (List Int)
timeDecoder =
    Json.list ("timeToStation" := Json.int)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }
