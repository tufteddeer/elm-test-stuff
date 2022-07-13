module StreamerInfo exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Carousel exposing (Msg)
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


type StreamerInfo
    = OnlineStreamer String String Int
    | OfflineStreamer String



{-
   View info about a streamer. uses the layout defined in streamerCardScaffhold
-}


streamerView : StreamerInfo -> Html msg
streamerView streamer =
    case streamer of
        OnlineStreamer name category viewers ->
            streamerCardScaffhold
                name
                (Just category)
                (div []
                    [ liveIndicator
                    , text
                        (numberString viewers)
                    ]
                )

        OfflineStreamer name ->
            streamerCardScaffhold
                name
                Nothing
                (text "Offline")



{-
   Layout to be used to view streamer infos, defines the layout used by streamerView
-}


streamerCardScaffhold : String -> Maybe String -> Html msg -> Html msg
streamerCardScaffhold titleContent subtitleContent rightContent =
    Grid.container []
        [ Grid.row []
            [ Grid.col [ Col.xsAuto ]
                [ profilePicture
                ]
            , Grid.col [ Col.sm ]
                [ Grid.row []
                    [ Grid.col [] [ strong [] [ text (maxLengthString 15 titleContent) ] ]
                    ]
                , Grid.row []
                    [ Grid.col [] [ text (Maybe.withDefault "" subtitleContent) ]
                    ]
                ]
            , Grid.col [ Col.xsAuto ]
                [ Grid.row []
                    [ Grid.col [] [ rightContent ] ]
                ]
            ]
        ]


main : Html msg
main =
    div [ style "width" "350px" ]
        [ CDN.stylesheet
        , Grid.container []
            [ streamerView (OnlineStreamer "Streamer*in" "Kategorie" 123)
            , streamerView (OnlineStreamer "Streamer*in" "Kategorie" 123456)
            , streamerView (OfflineStreamer "etwas langer name")
            ]
        ]
