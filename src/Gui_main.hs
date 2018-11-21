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
    one   <- UI.button    # set UI.text "1 Player"
    two   <- UI.button    # set UI.text "2 Player"
    three <- UI.button    # set UI.text "3 Player"
    four  <- UI.button    # set UI.text "4 Player"
    inputCity <- UI.input
    inputCountry <- UI.input
    inputRiver <- UI.input
    submit <- UI.button   # set UI.text "submit"
    main_content <- UI.div #. "main-content"
    randomLetterButton <- UI.button # set UI.text "Buchstabe bestimmen"


    -- input field for player one
    playerOneInput <- UI.div
        #. "input-field"
        #+ [ UI.p # set UI.text "Stadt"
           , element inputCity
           , UI.p # set UI.text "Land"
           , element inputCountry
           , UI.p # set UI.text "Fluss"
           , element inputRiver
           ]

    --on UI.click randomLetterButton $ \_ -> do
        --randomLetter <- randomRIO ('a','z')
        --element main_content #+ [UI.div # set UI.text "test"]

    -- click button 1 Player
    on UI.click one $ \_ -> do
        element main_content #+ [element playerOneInput, element submit]

    -- click button 2 Player
    on UI.click two $ \_ -> do
        element main_content #+ [element playerOneInput, element submit]

    -- click button 3 Player
    on UI.click three $ \_ -> do
        element main_content #+ [element playerOneInput, element submit]

    -- click button 4 Player
    on UI.click four $ \_ -> do
        element main_content #+ [element playerOneInput, element submit]

    -- click button submit
    on UI.click submit $ \_ -> do
        city <- get value inputCity
        country <- get value inputCountry
        river <- get value inputRiver
        element main_content #+ [string city, string country, string river]

    -- visual style
    UI.div #. "main_content" #+
        [ UI.ul #. "button-group round" #+
            map (\x -> UI.li #+ [element x]) [one, two, three, four],
            element main_content
        ]
