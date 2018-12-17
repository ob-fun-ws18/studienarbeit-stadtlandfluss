module GuiMain where

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import GameLogic

import System.Random
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

guiMain = startGUI defaultConfig { jsStatic = Just "." } setup

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


    table <- UI.table
        #. "table"
        #+ [ UI.tr #+
            [ UI.th
            , UI.th # set UI.text "Spieler 1"
            , UI.th # set UI.text "Spieler 2"
            , UI.th # set UI.text "Spieler 3"
            , UI.th # set UI.text "Spieler 4"
            ]
        , UI.tr #+
            [ UI.td # set UI.text "Stadt"
            , UI.td #+
                [ element inputCity1 ]
            , UI.td #+
                [ element inputCity2 ]
            , UI.td #+
                [ element inputCity3 ]
            , UI.td #+
                [ element inputCity4 ]
            ]
        , UI.tr #+
            [ UI.td # set UI.text "Land"
            , UI.td #+
                [ element inputCountry1 ]
            , UI.td #+
                [ element inputCountry2 ]
            , UI.td #+
                [ element inputCountry3 ]
            , UI.td #+
                [ element inputCountry4 ]
            ]
        , UI.tr #+
            [ UI.td # set UI.text "Fluss"
            , UI.td #+
                [ element inputRiver1 ]
            , UI.td #+
                [ element inputRiver2 ]
            , UI.td #+
                [ element inputRiver3 ]
            , UI.td #+
                [ element inputRiver4 ]
            ]
        ]


    -- click button submit
    on UI.click submit $ \_ -> do
        let cities = checkCity [get value inputCity1, get value inputCity2, get value inputCity3, get value inputCity4]
        let countries = checkCountry [get value inputCountry1, get value inputCountry2, get value inputCountry3, get value inputCountry4]
        let rivers = checkRiver [get value inputRiver1, get value inputRiver2, get value inputRiver3, get value inputRiver4]

        element result #+
            [ UI.h5 # set UI.text "Ergebnis"
            , UI.tr #+
               [ UI.th
               , UI.th # set UI.text "Spieler 1"
               , UI.th # set UI.text "Spieler 2"
               , UI.th # set UI.text "Spieler 3"
               , UI.th # set UI.text "Spieler 4"
               ]
           , UI.tr #+
               [ UI.td # set UI.text "Stadt"
               , UI.td # set UI.text (snd (head cities))
               , UI.td # set UI.text (snd (cities!!1))
               , UI.td # set UI.text (snd (cities!!2))
               , UI.td # set UI.text (snd (cities!!3))
               ]
           , UI.tr #+
              [ UI.td # set UI.text "Land"
              , UI.td # set UI.text (snd (head countries))
              , UI.td # set UI.text (snd (countries!!1))
              , UI.td # set UI.text (snd (countries!!2))
              , UI.td # set UI.text (snd (countries!!3))
              ]
           , UI.tr #+
             [ UI.td # set UI.text "Fluss"
             , UI.td # set UI.text (snd (head rivers))
             , UI.td # set UI.text (snd (rivers!!1))
             , UI.td # set UI.text (snd (rivers!!2))
             , UI.td # set UI.text (snd (rivers!!3))
             ]
           ]

    -- visual style
    UI.div #. "main_content" #+
        [ element table
        , element result
        , element submit
        ]
