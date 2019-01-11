{-|
Module      : GuiMain
Description : Contains functions to create a GUI for the user to play

This module builds up the components and design of the GUI. The GUI is build with the
threepenny-gui library (http://hackage.haskell.org/package/threepenny-gui) which uses
HTML like statements in haskell to build up a GUI in a browser window which is hostet
on localhost.
-}
module GuiMain where

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import GameLogic

guiMain = startGUI defaultConfig { jsStatic = Just "." } setup

-- | Builds up the user window
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

-- | This defines the content inside the user window
mkDisplay :: UI Element
mkDisplay = do

    -- city inputs for all four players
    inputCity1 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity2 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity3 <- UI.input # set (attr "placeholder") "Stadt"
    inputCity4 <- UI.input # set (attr "placeholder") "Stadt"

    -- country inputs for all four players
    inputCountry1 <- UI.input # set (attr "placeholder") "Land"
    inputCountry2 <- UI.input # set (attr "placeholder") "Land"
    inputCountry3 <- UI.input # set (attr "placeholder") "Land"
    inputCountry4 <- UI.input # set (attr "placeholder") "Land"

    -- river inputs for all four players
    inputRiver1 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver2 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver3 <- UI.input # set (attr "placeholder") "Fluss"
    inputRiver4 <- UI.input # set (attr "placeholder") "Fluss"

    -- get a random letter which the players have to use for the first round
    randomLetter <- liftIO (getRandomChar)
    let letterString = [randomLetter]

    -- define the score fields and set them to 0 for the first round
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

    letter <- UI.input #. "score-field"
        # set UI.enabled False
        # set UI.value letterString

    -- define the letter field and set it to the random letter defined above for the first round
    letterField <- UI.div #. "letter" #+
        [ UI.p #. "letter-heading" # set UI.text ("Buchstabe")
        , element letter
        ]

    -- a submit button for the players to submit their answers
    submit <- UI.button   # set UI.text "submit"

    -- a result div container where result panels will be added to
    result <- UI.div #. "result" #+
        [ UI.h3 # set UI.text "Ergebnisse"]

    -- a score div container where score fields are added to
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

    -- a html table element
    -- contains input fields for city, country, and river for four players
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

    -- this gets triggered after the players press the submit button to
    -- submit their answers
    on UI.click submit $ \_ -> do

        -- read in the city inputs for all four users
        city1 <- get value inputCity1
        city2 <- get value inputCity2
        city3 <- get value inputCity3
        city4 <- get value inputCity4

        -- read in the country inputs for all four users
        country1 <- get value inputCountry1
        country2 <- get value inputCountry2
        country3 <- get value inputCountry3
        country4 <- get value inputCountry4

        -- read in the river inputs for all four users
        river1 <- get value inputRiver1
        river2 <- get value inputRiver2
        river3 <- get value inputRiver3
        river4 <- get value inputRiver4

        -- check if the answers are correct via api request -> GameLogic.hs
        cities <- liftIO (checkCity [city1, city2, city3,city4])
        countries <- liftIO (checkCountry [country1, country2, country3, country4])
        rivers <- liftIO (checkRiver [river1, river2, river3, river4])

        -- calculate the points of player 1 and add them to the total score
        oldScore1 <- get value score1
        let oldScoreInt1 = read oldScore1 :: Int
        let cityPoints1 = fst (head cities) :: Int
        let countryPoints1 = fst (head countries) :: Int
        let riverPoints1 = fst (head rivers) :: Int
        let newScore1 = oldScoreInt1 + cityPoints1 + countryPoints1 + riverPoints1

        -- calculate the points of player 2 and add them to the total score
        oldScore2 <- get value score2
        let oldScoreInt2 = read oldScore2 :: Int
        let cityPoints2 = fst (cities!!1) :: Int
        let countryPoints2 = fst (countries!!1) :: Int
        let riverPoints2 = fst (rivers!!1) :: Int
        let newScore2 = oldScoreInt2 + cityPoints2 + countryPoints2 + riverPoints2

        -- calculate the points of player 3 and add them to the total score
        oldScore3 <- get value score3
        let oldScoreInt3 = read oldScore3 :: Int
        let cityPoints3 = fst (cities!!2) :: Int
        let countryPoints3 = fst (countries!!2) :: Int
        let riverPoints3 = fst (rivers!!2) :: Int
        let newScore3 = oldScoreInt3 + cityPoints3 + countryPoints3 + riverPoints3

        -- calculate the points of player 4 and add them to the total score
        oldScore4 <- get value score4
        let oldScoreInt4 = read oldScore4 :: Int
        let cityPoints4 = fst (cities!!3) :: Int
        let countryPoints4 = fst (countries!!3) :: Int
        let riverPoints4 = fst (rivers!!3) :: Int
        let newScore4 = oldScoreInt4 + cityPoints4 + countryPoints4 + riverPoints4

        -- update the score fields for all four players
        element score1 # set UI.value (show newScore1)
        element score2 # set UI.value (show newScore2)
        element score3 # set UI.value (show newScore3)
        element score4 # set UI.value (show newScore4)

        -- define a new random letter for the next round
        randomLetter <- liftIO (getRandomChar)
        let letterString = [randomLetter]

        -- update the letter field
        element letter # set UI.value letterString

        -- add a result panel which shows the user how many points they got
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
                        [ UI.span #. "input" # set UI.text "Eingabe"
                        , UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "input" # set UI.text "Eingabe"
                        , UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "input" # set UI.text "Eingabe"
                        , UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    , UI.th #+
                        [ UI.span #. "input" # set UI.text "Eingabe"
                        , UI.span #. "points" # set UI.text "Punkte"
                        , UI.span #. "possibility" # set UI.text "Wahrscheinlichkeit"
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Stadt"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text city1
                        , UI.span #. "points" # set UI.text (show (fst (head cities)))
                        , UI.span #. "possibility" # set UI.text (snd (head cities))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text city2
                        , UI.span #. "points" # set UI.text (show (fst (cities!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text city3
                        , UI.span #. "points" # set UI.text (show (fst (cities!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text city4
                        , UI.span #. "points" # set UI.text (show (fst (cities!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (cities!!3))
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Land"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text country1
                        , UI.span #. "points" # set UI.text (show (fst (head countries)))
                        , UI.span #. "possibility" # set UI.text (snd (head countries))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text country2
                        , UI.span #. "points" # set UI.text (show (fst (countries!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text country3
                        , UI.span #. "points" # set UI.text (show (fst (countries!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text country4
                        , UI.span #. "points" # set UI.text (show (fst (countries!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (countries!!3))
                        ]
                    ]
                , UI.tr #+
                    [ UI.th #. "first-column" # set UI.text "Fluss"
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text river1
                        , UI.span #. "points" # set UI.text (show (fst (head rivers)))
                        , UI.span #. "possibility" # set UI.text (snd (head rivers))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text river2
                        , UI.span #. "points" # set UI.text (show (fst (rivers!!1)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!1))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text river3
                        , UI.span #. "points" # set UI.text (show (fst (rivers!!2)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!2))
                        ]
                    , UI.td #. "result-field" #+
                        [ UI.span #. "input" # set UI.text river4
                        , UI.span #. "points" # set UI.text (show (fst (rivers!!3)))
                        , UI.span #. "possibility" # set UI.text (snd (rivers!!3))
                        ]
                    ]
                ]
            ]

        -- reset the city input fields so the user doesn't have to delete them manually
        element inputCity1 # set UI.value ""
        element inputCity2 # set UI.value ""
        element inputCity3 # set UI.value ""
        element inputCity4 # set UI.value ""

        -- reset the country input fields so the user doesn't have to delete them manually
        element inputCountry1 # set UI.value ""
        element inputCountry2 # set UI.value ""
        element inputCountry3 # set UI.value ""
        element inputCountry4 # set UI.value ""

        -- reset the river input fields so the user doesn't have to delete them manually
        element inputRiver1 # set UI.value ""
        element inputRiver2 # set UI.value ""
        element inputRiver3 # set UI.value ""
        element inputRiver4 # set UI.value ""

    -- add the above created html elements to the main content element
    UI.div #. "main_content" #+
        [ element result
        , element score
        , element letterField
        , element table
        , element submit
        ]
