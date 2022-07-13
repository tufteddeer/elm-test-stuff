module InteractiveTime exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser
import RFC3339 exposing (DateTime, zulu)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { timestamp : String
    , dateTime : DateTime
    }


init : Model
init =
    { timestamp = ""
    , dateTime = { date = { day = 0, month = 0, year = 0 }, time = { hours = 0, minutes = 0, seconds = 0 }, offset = zulu }
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            let
                dateTime =
                    case Parser.run RFC3339.dateTimeParser newContent of
                        Err _ ->
                            Debug.todo "error"

                        Ok newDateTime ->
                            newDateTime
            in
            { model | timestamp = newContent, dateTime = dateTime }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Timestamp", value model.timestamp, onInput Change ] []
        , div [] [ text (Debug.toString model.dateTime) ]
        , div [] [ text ("Datum: " ++ String.fromInt model.dateTime.date.day ++ "." ++ String.fromInt model.dateTime.date.month ++ "." ++ String.fromInt model.dateTime.date.year) ]
        , div [] [ text ("Uhrzeit: " ++ String.fromInt model.dateTime.time.hours ++ ":" ++ String.fromInt model.dateTime.time.minutes ++ ":" ++ String.fromInt model.dateTime.time.seconds) ]
        , div [] [ text ("Offset: " ++ Debug.toString model.dateTime.offset) ]
        ]
