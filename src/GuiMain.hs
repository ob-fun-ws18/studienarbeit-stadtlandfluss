module GuiMain where

import Data.IORef

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import GameLogic

import System.Random
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

guiMain = startGUI defaultConfig { jsStatic = Just "." } setup

round :: Int
round = 1

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Stadt - Land - Fluss"
    UI.addStyleSheet window "gui.css"

    header <- UI.div
        #. "header"
        #+  [ UI.h1     # set UI.text "Stadt - Land - Fluss"]

    getBody window #+
        [ UI.div #. "content" #+
            [ element header,
              mkDisplay
            ]
        ]
    return ()


mkDisplay :: UI Element
mkDisplay = do

    -- city inputs
    inputCity1 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity2 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity3 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity4 <- UI.input # set (attr "placeholder") "Stadt"

    -- country inputs
    inputCountry1 <- UI.input # set (attr "placeholder") "Land"
    inputCountry2 <- UI.input # set (attr "placeholder") "Land"
    inputCountry3 <- UI.input # set (attr "placeholder") "Land"
    inputCountry4 <- UI.input # set (attr "placeholder") "Land"

    -- river inputs
    inputRiver1 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver2 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver3 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver4 <- UI.input # set (attr "placeholder") "Fluss"

    submit <- UI.button   # set UI.text "submit"
    result <- UI.div #. "result"
    randomLetterButton <- UI.button # set UI.text "Buchstabe bestimmen"

    score <- UI.div #. "score" #+
        [ UI.p #. "scores-heading" # set UI.text ("Runde 1")
        , UI.span #. "scores" # set UI.text ("Spieler 1 Punkte: " ++(show (currentScores!!0)))
        , UI.span #. "scores" # set UI.text ("Spieler 2 Punkte: " ++(show (currentScores!!1)))
        , UI.span #. "scores" # set UI.text ("Spieler 3 Punkte: " ++(show (currentScores!!2)))
        , UI.span #. "scores" # set UI.text ("Spieler 4 Punkte: " ++(show (currentScores!!3)))
        ]

    table <- UI.table #. "input-table" #+
        [ UI.tr #+
            [ UI.th
            , UI.th # set UI.text "Spieler 1"
            , UI.th # set UI.text "Spieler 2"
            , UI.th # set UI.text "Spieler 3"
            , UI.th # set UI.text "Spieler 4"
            ]
        , UI.tr #+
            [ UI.th # set UI.text "Stadt"
            , UI.td #. "input-field" #+
                [ element inputCity1 ]
            , UI.td #. "input-field" #+
                [ element inputCity2 ]
            , UI.td #. "input-field" #+
                [ element inputCity3 ]
            , UI.td #. "input-field" #+
                [ element inputCity4 ]
            ]
        , UI.tr #+
            [ UI.th # set UI.text "Land"
            , UI.td #. "input-field" #+
                [ element inputCountry1 ]
            , UI.td #. "input-field" #+
                [ element inputCountry2 ]
            , UI.td #. "input-field" #+
                [ element inputCountry3 ]
            , UI.td #. "input-field" #+
                [ element inputCountry4 ]
            ]
        , UI.tr #+
            [ UI.th # set UI.text "Fluss"
            , UI.td #. "input-field" #+
                [ element inputRiver1 ]
            , UI.td #. "input-field" #+
                [ element inputRiver2 ]
            , UI.td #. "input-field" #+
                [ element inputRiver3 ]
            , UI.td #. "input-field" #+
                [ element inputRiver4 ]
            ]
        ]

    -- click button submit
    on UI.click submit $ \_ -> do

        city1 <- get value inputCity1
        city2 <- get value inputCity2
        city3 <- get value inputCity3
        city4 <- get value inputCity4

        country1 <- get value inputCountry1
        country2 <- get value inputCountry2
        country3 <- get value inputCountry3
        country4 <- get value inputCountry4

        river1 <- get value inputRiver1
        river2 <- get value inputRiver2
        river3 <- get value inputRiver3
        river4 <- get value inputRiver4

        cities <- liftIO (checkCity [city1, city2, city3,city4])
        countries <- liftIO (checkCountry [country1, country2, country3, country4])
        rivers <- liftIO (checkRiver [river1, river2, river3, river4])

        element score #+
            [ UI.p #. "scores-heading" # set UI.text "Runde 2"
            , UI.span #. "scores" # set UI.text ("Spieler 1 Punkte: " ++(show (currentScores!!0)))
            , UI.span #. "scores" # set UI.text ("Spieler 2 Punkte: " ++(show (currentScores!!1)))
            , UI.span #. "scores" # set UI.text ("Spieler 3 Punkte: " ++(show (currentScores!!2)))
            , UI.span #. "scores" # set UI.text ("Spieler 4 Punkte: " ++(show (currentScores!!3)))
            ]

        element result #+
            [ UI.h5 #. "header-result" # set UI.text "Ergebnis"
            , UI.table #. "result-table" #+
                [ UI.tr #+
                    [ UI.th #. "player"
                    , UI.th #. "player" # set UI.text "Spieler 1"
                    , UI.th #. "player" # set UI.text "Spieler 2"
                    , UI.th #. "player" # set UI.text "Spieler 3"
                    , UI.th #. "player" # set UI.text "Spieler 4"
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column"
                    , UI.th #+
                        [ UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Stadt"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (head cities)))
                        , UI.span #. "possibility" # set UI.text (snd (head cities))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (cities!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (cities!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (cities!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!3))
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Land"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (head countries)))
                        , UI.span #. "possibility" # set UI.text (snd (head countries))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (countries!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (countries!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (countries!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!3))
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Fluss"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (head rivers)))
                        , UI.span #. "possibility" # set UI.text (snd (head rivers))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (rivers!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (rivers!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "points" # set UI.text (show (fst (rivers!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!3))
                        ]
                    ]
                ]
            ]

    -- visual style
    UI.div #. "main_content" #+
        [ element score
        , element table
        , element result
        , element submit
        ]
