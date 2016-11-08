module Main exposing(..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=))
import Http exposing (get)
import Task

type alias Model =
    { times: List Int }

type Msg =
    ResultSuccess (List Int)
    | ResultFailed Http.Error
    | Refresh

init : ( Model, Cmd Msg )
init =
  ( Model [], getArrivalTime )

view: Model -> Html Msg
view model =
    let
        t = (model.times
            |> List.head
            |> (Maybe.withDefault 0)) // 60
    in
        div
            [ class "container" ]
            [ div
                [ class "minutes" ]
                [ text (toString t)]
            , div
                [ class "refresh" ]
                [ button
                    [ onClick Refresh ]
                    [ text "Refresh" ]
                ]
            ]

update : Msg -> Model -> (Model,  Cmd Msg)
update msg model =
    case msg of
        ResultSuccess times ->
            ( {model | times = (List.reverse times) }, Cmd.none )

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

