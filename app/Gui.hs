module Gui where
import qualified Graphics.UI.Gtk as Gtk3
import qualified Data.Text as T

createWindow :: String -> IO ()
createWindow s = do
     -- Gtk3 Gui
    Gtk3.initGUI
    -- Create Window
    window <- Gtk3.windowNew
    Gtk3.on window Gtk3.objectDestroy Gtk3.mainQuit
    Gtk3.set window [ Gtk3.containerBorderWidth Gtk3.:= 10, Gtk3.windowTitle Gtk3.:= s ]

    hBox <- Gtk3.hBoxNew True 5
    hbuttonbox <- Gtk3.hButtonBoxNew
    vBox <- Gtk3.vBoxNew True 5
    hBox2 <- Gtk3.hBoxNew True 5

    Gtk3.containerAdd vBox hBox2
    Gtk3.containerAdd vBox hBox
    Gtk3.containerAdd vBox hbuttonbox
    Gtk3.containerAdd window vBox

    checkCustom <- Gtk3.checkButtonNewWithLabel "Custom Mode"
    Gtk3.on checkCustom Gtk3.toggled onCustomChecked

    button1 <- Gtk3.buttonNewWithLabel "Select output directory"
    button2 <- Gtk3.buttonNewWithLabel "Create File"

    comboBox <- Gtk3.comboBoxNewText
    Gtk3.comboBoxAppendText comboBox (T.pack "Twelve Bar Blues")
    Gtk3.comboBoxAppendText comboBox (T.pack "Axis of Awesome")
    Gtk3.comboBoxAppendText comboBox (T.pack "Pessimistic")
    Gtk3.comboBoxAppendText comboBox (T.pack "Pop")
    Gtk3.comboBoxAppendText comboBox (T.pack "JazzCat")
    Gtk3.comboBoxAppendText comboBox (T.pack "Pachelbel")
    Gtk3.comboBoxSetActive comboBox 0

    entChord <- Gtk3.entryNew
    Gtk3.entrySetText entChord "Enter chord array"
    labChord <- Gtk3.labelNew (Just "Chord progression:")

    Gtk3.containerAdd hBox2 checkCustom
    Gtk3.containerAdd hBox labChord
    Gtk3.containerAdd hBox entChord
    Gtk3.containerAdd hBox comboBox

    Gtk3.set hbuttonbox [ Gtk3.containerChild Gtk3.:= button
                          | button <- [button1, button2] ]

    Gtk3.set hbuttonbox [ Gtk3.buttonBoxLayoutStyle Gtk3.:= Gtk3.ButtonboxStart
                           , Gtk3.buttonBoxChildSecondary button2 Gtk3.:= True  ]

    Gtk3.widgetShowAll window

    Gtk3.mainGUI 

onCustomChecked :: IO ()
onCustomChecked = do
    putStrLn "Test"