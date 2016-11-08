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
    { times : List Int
    , loading: Bool }


type Msg
    = ResultSuccess (List Int)
    | ResultFailed Http.Error
    | Refresh


init : ( Model, Cmd Msg )
init =
    ( Model [] True, getArrivalTime )

pad =
    toString >> padLeft 2 '0'

secondsToMinutes t =
    let
        m =
            t // 60 |> pad

        s =
            rem t 60 |> pad
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
            , div
                [ class "refresh"
                , disabled model.loading ]
                [ button [ onClick Refresh ]
                    [ text (if model.loading then "Loading..." else "Refresh") ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultSuccess times ->
            ( { model | times = times |> List.sort
            , loading = False }, Cmd.none )

        ResultFailed err ->
            ( { model | loading = False }, Cmd.none )

        Refresh ->
            ( { model | loading = True }, getArrivalTime )


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
