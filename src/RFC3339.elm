module RFC3339 exposing (..)

{-| This module defines types parsers for a subset of RFC3339.
-}

import Html exposing (text)
import Parser exposing ((|.), (|=), Parser, int, map, oneOf, spaces, succeed, symbol)


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }



{-
   Parse a YYYY-MM-DD date ("full-date")
-}


dateParser : Parser Date
dateParser =
    succeed Date
        -- year
        |= int
        |. symbol "-"
        -- month
        |= paddedIntParser
        |. symbol "-"
        -- day
        |= paddedIntParser


type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


{-| Parse a HH:MM:SS ("partial time" but without "time-secfrac")
-}
timeParser : Parser Time
timeParser =
    succeed Time
        -- hours
        |= paddedIntParser
        |. symbol ":"
        -- minutes
        |= paddedIntParser
        |. symbol ":"
        -- seconds
        |= paddedIntParser


type alias DateTime =
    { date : Date
    , time : Time
    , offset : Offset
    }



{-
   Parse a "date-time" string
-}


dateTimeParser : Parser DateTime
dateTimeParser =
    succeed DateTime
        |= dateParser
        -- TODO should be case insensitive
        |. symbol "T"
        |= timeParser
        |= offsetParser


type OffsetDirection
    = Positive
    | Negative


type alias Offset =
    { direction : OffsetDirection
    , hours : Int
    , minutes : Int
    }



{-
   Zulu is the +00:00 UTC offset
-}


zulu =
    { direction = Positive, hours = 0, minutes = 0 }


offsetDirectionParser : Parser OffsetDirection
offsetDirectionParser =
    succeed identity
        |= oneOf
            [ Parser.map (\_ -> Positive)
                (symbol
                    "+"
                )
            , Parser.map
                (\_ -> Negative)
                (symbol
                    "-"
                )
            ]


zOffsetParser : Parser Offset
zOffsetParser =
    Parser.map (\_ -> { direction = Positive, hours = 0, minutes = 0 }) (symbol "Z")


offsetParser : Parser Offset
offsetParser =
    oneOf
        [ succeed Offset
            |= offsetDirectionParser
            |= paddedIntParser
            |. symbol ":"
            |= paddedIntParser
        , zOffsetParser
        ]



{-
   parse integers that may be zero padded (once, like "01", "0123",..)
-}


paddedIntParser : Parser Int
paddedIntParser =
    oneOf
        [ succeed identity
            |. symbol "0"
            -- TOOD: using paddedIntParser recursively
            -- could allow parsing more than one leading 0
            |= int
        , succeed identity
            |= int
        ]


main =
    text <| Debug.toString (Parser.run offsetDirectionParser "+3")
