module Gui_main where

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import System.Random
{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

gui_main = startGUI defaultConfig { jsStatic = Just "." } setup

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


    table <- UI.div
        #. "table"
        #+ [ UI.tr #+
            [ UI.th # set UI.text "Spieler 1"
            , UI.th # set UI.text "Spieler 2"
            , UI.th # set UI.text "Spieler 3"
            , UI.th # set UI.text "Spieler 4"
            ]
        , UI.tr #+
            [ UI.td #+
                [ element inputCity1 ]
            , UI.td #+
                [ element inputCity2 ]
            , UI.td #+
                [ element inputCity3 ]
            , UI.td #+
                [ element inputCity4 ]
            ]
        , UI.tr #+
            [ UI.td #+
                [ element inputCountry1 ]
            , UI.td #+
                [ element inputCountry2 ]
            , UI.td #+
                [ element inputCountry3 ]
            , UI.td #+
                [ element inputCountry4 ]
            ]
        , UI.tr #+
            [ UI.td #+
                [ element inputRiver1 ]
            , UI.td #+
                [ element inputRiver2 ]
            , UI.td #+
                [ element inputRiver3 ]
            , UI.td #+
                [ element inputRiver4 ]
            ]
        ]


    --on UI.click randomLetterButton $ \_ -> do
        --randomLetter <- randomRIO ('a','z')
        --element main_content #+ [UI.div # set UI.text "test"]

    -- click button submit
    on UI.click submit $ \_ -> do
        city <- get value inputCity1
        country <- get value inputCountry1
        river <- get value inputRiver1
        element result #+ [string city, string country, string river]

    -- visual style
    UI.div #. "main_content" #+
        [ element table
        , element result
        , element submit
        ]
