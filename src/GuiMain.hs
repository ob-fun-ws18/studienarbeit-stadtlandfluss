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

    randomLetter <- liftIO (getRandomChar)
    let letterString = [randomLetter]

    -- scores
    score1 <- UI.input #. "score-field"
        # set UI.enabled False
        # set UI.value "0"

    score2 <- UI.input #. "score-field"
        # set UI.enabled False
        # set UI.value "0"

    score3 <- UI.input #. "score-field"
        # set UI.enabled False
        # set UI.value "0"

    score4 <- UI.input #. "score-field"
        # set UI.enabled False
        # set UI.value "0"

    letter <- UI.input
        # set UI.enabled False
        # set UI.value letterString

    submit <- UI.button   # set UI.text "submit"
    result <- UI.div #. "result" #+
        [ UI.h3 # set UI.text "Ergebnisse"]
    randomLetterButton <- UI.button # set UI.text "Buchstabe bestimmen"

    score <- UI.div #. "score" #+
        [ UI.p #. "scores-heading" # set UI.text ("Punkte")
        , UI.span #. "scores" # set UI.text "Spieler 1: " #+
            [ element score1 ]
        , UI.span #. "scores" # set UI.text "Spieler 2: " #+
            [ element score2 ]
        , UI.span #. "scores" # set UI.text "Spieler 3: " #+
            [ element score3 ]
        , UI.span #. "scores" # set UI.text "Spieler 4: " #+
            [ element score4 ]
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

        oldScore1 <- get value score1
        let oldScoreInt1 = read oldScore1 :: Int
        let cityPoints1 = fst (head cities) :: Int
        let countryPoints1 = fst (head countries) :: Int
        let riverPoints1 = fst (head rivers) :: Int
        let newScore1 = oldScoreInt1 + cityPoints1 + countryPoints1 + riverPoints1

        oldScore2 <- get value score2
        let oldScoreInt2 = read oldScore2 :: Int
        let cityPoints2 = fst (cities!!1) :: Int
        let countryPoints2 = fst (countries!!1) :: Int
        let riverPoints2 = fst (rivers!!1) :: Int
        let newScore2 = oldScoreInt2 + cityPoints2 + countryPoints2 + riverPoints2

        oldScore3 <- get value score3
        let oldScoreInt3 = read oldScore3 :: Int
        let cityPoints3 = fst (cities!!2) :: Int
        let countryPoints3 = fst (countries!!2) :: Int
        let riverPoints3 = fst (rivers!!2) :: Int
        let newScore3 = oldScoreInt3 + cityPoints3 + countryPoints3 + riverPoints3

        oldScore4 <- get value score4
        let oldScoreInt4 = read oldScore4 :: Int
        let cityPoints4 = fst (cities!!3) :: Int
        let countryPoints4 = fst (countries!!3) :: Int
        let riverPoints4 = fst (rivers!!3) :: Int
        let newScore4 = oldScoreInt4 + cityPoints4 + countryPoints4 + riverPoints4

        element score1 # set UI.value (show newScore1)
        element score2 # set UI.value (show newScore2)
        element score3 # set UI.value (show newScore3)
        element score4 # set UI.value (show newScore4)

        randomLetter <- liftIO (getRandomChar)
        let letterString = [randomLetter]

        element letter # set UI.value letterString

        element result #+
            [ UI.h5 #. "header-result"
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
        [ element result
        , element score
        , element letter
        , element table
        , element submit
        ]
