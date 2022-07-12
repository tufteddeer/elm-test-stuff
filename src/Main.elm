module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Bootstrap.Table exposing (Row)
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (class, height, href, src, style, width)



{-
   Cut a string to have maximal n characters including trailing "..."
-}


maxLengthString : Int -> String -> String
maxLengthString n s =
    let
        len =
            String.length s
    in
    if len <= n then
        s

    else
        -- TODO: what about n < 3?
        String.dropRight ((len - n) + 3) s ++ "..."


numberString : Int -> String
numberString n =
    if n < 1000 then
        String.fromInt n

    else
        String.fromInt (floor (toFloat n / 1000)) ++ "K"


profilePicture : Html msg
profilePicture =
    img
        [ src "https://thispersondoesnotexist.com/image?x="
        , width 50
        , Border.circle
        ]
        []


liveIndicator : Html msg
liveIndicator =
    div
        [ style "background-color" "red"
        , style "height" "10px"
        , style "width" "10px"
        , style "display" "inline-block"
        , style "margin-right" "5px"
        , Border.circle
        ]
        []



-- TODO: offline streamers dont have categories


streamerCard : String -> String -> Maybe Int -> Html msg
streamerCard name category liveViewers =
    let
        liveOrOffline =
            case liveViewers of
                Nothing ->
                    text "Offline"

                Just x ->
                    div []
                        [ liveIndicator
                        , text
                            (numberString x)
                        ]
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.xsAuto ]
                [ profilePicture
                ]
            , Grid.col [ Col.sm ]
                [ Grid.row []
                    [ Grid.col [] [ strong [] [ text (maxLengthString 15 name) ] ]
                    ]
                , Grid.row []
                    [ Grid.col [] [ text category ]
                    ]
                ]
            , Grid.col [ Col.xsAuto ]
                [ Grid.row []
                    [ Grid.col [] [ liveOrOffline ] ]
                ]
            ]
        ]


main : Html msg
main =
    div [ style "width" "350px" ]
        [ CDN.stylesheet
        , Grid.container []
            [ streamerCard "Streamer*in" "Kategorie" (Just 123)
            , streamerCard "Streamer*in" "Kategorie" (Just 123456)
            , streamerCard "etwas langer name" "Kategorie" Nothing
            ]
        ]
