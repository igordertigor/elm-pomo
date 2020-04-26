port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, audio, button, div, text)
import Html.Attributes exposing (class, controls, id, src, style)
import Html.Events exposing (onClick)
import Json.Encode as E
import Task
import Time


port play : E.Value -> Cmd msg


secondSoundUri : String
secondSoundUri =
    "http://soundbible.com/grab.php?id=825&type=mp3"


firstSoundUri : String
firstSoundUri =
    "http://soundbible.com/grab.php?id=787&type=mp3"


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { startedAt : Time.Posix
    , currentTime : Time.Posix
    , intervalDuration : Maybe Int
    , nextBeep : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Time.millisToPosix 0) (Time.millisToPosix 0) Nothing (Time.millisToPosix 0), Cmd.none )


type Msg
    = StartInterval Time.Posix
    | StopInterval
    | PrepareInterval Int
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrepareInterval duration ->
            ( { model
                | intervalDuration = Just duration
                , nextBeep = incrementTime model.currentTime duration
              }
            , Task.perform StartInterval Time.now
            )

        StartInterval newTime ->
            ( { model
                | currentTime = newTime
                , startedAt = newTime
              }
            , Cmd.none
            )

        StopInterval ->
            ( { model | intervalDuration = Nothing }, Cmd.none )

        Tick newTime ->
            processTick model newTime


processTick : Model -> Time.Posix -> ( Model, Cmd Msg )
processTick model newTime =
    case model.intervalDuration of
        Nothing ->
            ( { model | currentTime = newTime }, Cmd.none )

        Just duration ->
            if compareTimes newTime model.nextBeep then
                ( { model
                    | currentTime = newTime
                    , nextBeep = snooze newTime
                  }
                , play (E.bool True)
                )

            else
                ( { model | currentTime = newTime }, Cmd.none )


snooze : Time.Posix -> Time.Posix
snooze t =
    incrementTime t (3 * minutes)


incrementTime : Time.Posix -> Int -> Time.Posix
incrementTime t ms =
    Time.millisToPosix (Time.posixToMillis t + ms)


compareTimes : Time.Posix -> Time.Posix -> Bool
compareTimes t s =
    Time.posixToMillis t > Time.posixToMillis s


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


view : Model -> Html Msg
view model =
    case model.intervalDuration of
        Just duration ->
            div [ class "pomodori" ]
                [ div [ id "timer" ]
                    [ text (formatTime model)
                    , text "/"
                    , text (timeAsString (Time.millisToPosix duration))
                    , audio
                        [ id "beep"
                        , src (getSoundUri model duration)
                        , controls False
                        ]
                        []
                    ]
                , div [] [ button [ onClick StopInterval ] [ text "Done" ] ]
                ]

        Nothing ->
            div [ class "pomodori" ]
                [ div [] [ text "Select an interval" ]
                , div []
                    [ intervalButton 25 "Work"
                    , intervalButton 5 "Short Break"
                    , intervalButton 20 "Long Break"
                    ]
                ]


getSoundUri : Model -> Int -> String
getSoundUri model duration =
    if Time.posixToMillis (timeDiff model.currentTime model.startedAt) > duration + 1000 then
        secondSoundUri

    else
        firstSoundUri


intervalButton : Int -> String -> Html Msg
intervalButton min label =
    button [ onClick (PrepareInterval (min * minutes)) ] [ text label ]


minutes : Int
minutes =
    60 * 1000


formatTime : Model -> String
formatTime model =
    let
        currentTime =
            model.currentTime

        startedAt =
            model.startedAt
    in
    timeAsString (timeDiff currentTime startedAt)


timeAsString : Time.Posix -> String
timeAsString t =
    let
        hours =
            Time.toHour Time.utc t
    in
    (if hours > 0 then
        padInt (Time.toHour Time.utc t) ++ ":"

     else
        ""
    )
        ++ padInt (Time.toMinute Time.utc t)
        ++ ":"
        ++ padInt (Time.toSecond Time.utc t)


padInt : Int -> String
padInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


timeDiff : Time.Posix -> Time.Posix -> Time.Posix
timeDiff t s =
    let
        diffMillis =
            Time.posixToMillis t - Time.posixToMillis s
    in
    Time.millisToPosix diffMillis
