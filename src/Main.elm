module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (field)
import Http exposing (Error(BadUrl, NetworkError, Timeout, BadStatus, BadPayload), send, get)
import Task
import String exposing (join, padLeft)
import Time exposing (every, second)


type alias Model =
    { times : List Int
    , loading: Bool
    , error: Maybe String }


type Msg
    = ResultReceived (Result Http.Error (List Int))
    | Refresh


init : ( Model, Cmd Msg )
init =
    ( Model [] True Nothing, getArrivalTime )

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
    div [ class "container" ]
        [ div []
            (List.map timeToBus model.times )
        , div
            [ class "refresh"
            , disabled model.loading ]
            [ button [ onClick Refresh ]
                [ text (if model.loading then "Loading..." else "Refresh") ]
            ]
        , div
            [ class "error" ]
            [ text (Maybe.withDefault "" model.error) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultReceived (Ok times) ->
            ( { model | times = times |> List.sort
            , loading = False
            , error = Nothing }, Cmd.none )

        ResultReceived (Err err) ->
            let
                errStr =
                    case err of
                        Timeout -> "The service timed out"
                        NetworkError -> "There was some sort of network error"
                        BadUrl _ -> "There was a bad url error"
                        BadStatus _ -> "Bad Status Error"
                        BadPayload _ _ -> "Bad payload error"
            in
            ( { model | loading = False
             , error = errStr |> Just }, Cmd.none )

        Refresh ->
            ( { model | loading = True }, getArrivalTime )


getArrivalTime : Cmd Msg
getArrivalTime =
    send ResultReceived
        <| get "https://api.tfl.gov.uk/StopPoint/490010849S/Arrivals?app_id=da7d2b38&app_key=6e41a466bf61c85ce50712c5622e12d1" timeDecoder


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
