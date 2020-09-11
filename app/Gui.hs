module Gui where
import qualified Graphics.UI.Gtk as Gtk3
import qualified Data.Text as T
import Data.Text.Conversions
import Control.Lens
import Structures
import Music
import Data.Maybe
import System.Directory
import System.Glib.UTFString
import Foreign.C.String

{-# LANGUAGE OverloadedStrings #-}

data GuiElements = GuiElements {
    _window :: Gtk3.Window,
    _hBox :: Gtk3.HBox,
    _checkCustom :: Gtk3.CheckButton,
    _comboBox :: Gtk3.ComboBox,
    _entChord :: Gtk3.Entry,
    _butSave :: Gtk3.Button,
    _butStart :: Gtk3.Button
}

--window :: Lens' GuiElements Gtk3.Window
--window = lens _window (\guiElements g -> guiElements { _window = g })

window :: Lens' GuiElements Gtk3.Window
window k (GuiElements win hbox che com ent butSave butStart) = (\win' -> GuiElements win' hbox che com ent butSave butStart) <$> k win

setupComboBox :: Gtk3.ComboBox -> [String] -> [IO Int]
setupComboBox comboBox strings = map (\x -> Gtk3.comboBoxAppendText comboBox (T.pack x)) strings

addWidgets ::(Gtk3.WidgetClass widgets, Gtk3.ContainerClass container) => [widgets] -> container -> [IO ()]
addWidgets widgets container = map (\x -> Gtk3.containerAdd (Gtk3.toContainer container) (Gtk3.toWidget x)) widgets

toFilePath :: [Char] -> FilePath
toFilePath a = a

getFilePath :: IO(String)
getFilePath = do
    a <- getCurrentDirectory
    return $ a ++ (toFilePath "/output.wav") 

initialiseGui :: ([Progression] -> Key -> Octave -> Int -> Int -> IO ()) -> String -> IO (GuiElements)
initialiseGui createSheet title = do
    Gtk3.initGUI
    -- Get Current working directory
    filepath <- getFilePath

    -- Create GUI elements
    window <- Gtk3.windowNew
    hBox <- Gtk3.hBoxNew True 5
    hbuttonbox <- Gtk3.hButtonBoxNew
    vBox <- Gtk3.vBoxNew True 5
    hBox2 <- Gtk3.hBoxNew True 5
    hBox3 <- Gtk3.hBoxNew True 5
    button1 <- Gtk3.buttonNewWithLabel "Select output directory"
    button2 <- Gtk3.buttonNewWithLabel "Create File"
    comboBox <- Gtk3.comboBoxNewText
    comboBox2 <- Gtk3.comboBoxNewText
    entChord <- Gtk3.entryNew
    labChord <- Gtk3.labelNew (Just "Chord progression:")
    labFolderHead <- Gtk3.labelNew (Just "Filename:")
    labFolder <- Gtk3.labelNew (Just filepath)
    checkCustom <- Gtk3.checkButtonNewWithLabel "Custom Mode"

    -- Setup GUI elements
    Gtk3.on window Gtk3.objectDestroy Gtk3.mainQuit
    Gtk3.set window [ Gtk3.containerBorderWidth Gtk3.:= 10, Gtk3.windowTitle Gtk3.:= title ]
    Gtk3.entrySetText entChord "Enter chord array"

    Gtk3.containerAdd vBox hBox2
    Gtk3.containerAdd vBox hBox
    Gtk3.containerAdd vBox hbuttonbox
    Gtk3.containerAdd vBox hBox3
    Gtk3.containerAdd window vBox
    Gtk3.containerAdd hBox2 checkCustom
    Gtk3.containerAdd hBox labChord
    Gtk3.containerAdd hBox entChord
    Gtk3.containerAdd hBox comboBox
    Gtk3.containerAdd hBox comboBox2
    Gtk3.containerAdd hBox3 labFolderHead
    Gtk3.containerAdd hBox3 labFolder
    
    Gtk3.set hbuttonbox [ Gtk3.containerChild Gtk3.:= button
                          | button <- [button1, button2] ]
    Gtk3.set hbuttonbox [ Gtk3.buttonBoxLayoutStyle Gtk3.:= Gtk3.ButtonboxStart
                           , Gtk3.buttonBoxChildSecondary button2 Gtk3.:= True  ]

    sequence $ setupComboBox comboBox ["Twelve Bar Blues", "Axis of Awesome", "Pessimistic", "Pop", "JazzCat", "Pachelbel"]
    sequence $ setupComboBox comboBox2 ["Minor", "Major"]
    Gtk3.comboBoxSetActive comboBox 0
    Gtk3.comboBoxSetActive comboBox2 0

    -- Setup event handlers
    Gtk3.on button1 Gtk3.buttonActivated $ openSelectFolderDialog window labFolder
    Gtk3.on button2 Gtk3.buttonActivated $ onStartButtonClicked comboBox comboBox2 labFolder createSheet
    Gtk3.on checkCustom Gtk3.toggled (onCustomChecked hBox entChord comboBox checkCustom)

    -- Finish GUI setup
    Gtk3.widgetShowAll window
    Gtk3.containerRemove hBox entChord
    Gtk3.mainGUI

    let elements = GuiElements {_window = window,_hBox = hBox,_checkCustom = checkCustom, _comboBox = comboBox,_entChord = entChord, _butSave = button1, _butStart = button2}
    return elements



createWindow :: String -> IO ()
createWindow s = do
    --windowElements <- initialiseGui (test) s
    
    --size <- Gtk3.windowGetSize (_window windowElements)

    -- Adding listeners after gui creation does not work
    --Gtk3.on (_checkCustom windowElements) Gtk3.toggled (onCustomChecked (_hBox windowElements) (_entChord windowElements) (_comboBox windowElements) (_checkCustom windowElements))

    --Gtk3.on _checkCustom windowElements Gtk3.toggled onCustomChecked
    return ()

fuckOff :: String -> IO(Maybe T.Text)
fuckOff text = do return (Just (convertText (text :: String) :: T.Text))

toMaybe :: String -> Maybe T.Text
toMaybe text = Just( toText text )

chooseChord :: String -> IO()
chooseChord chordType = do
    if chordType == "Major"
        then putStrLn "Major"
        else putStrLn "Minor"


onStartButtonClicked :: Gtk3.ComboBox -> Gtk3.ComboBox -> Gtk3.Label -> ([Progression] -> Key -> Octave -> Int -> Int -> IO ()) -> IO()
onStartButtonClicked comboBox comboBoxMiMa fileLabel createSheet = do
    filepath <- (Gtk3.labelGetText fileLabel) :: IO([Char])

    chordTypeText <- Gtk3.comboBoxGetActiveText comboBoxMiMa
    let chordType = convertText((fromMaybe (toText "Major") chordTypeText) :: T.Text) :: String

    coText <- Gtk3.comboBoxGetActiveText comboBox
    let text = fromMaybe (toText "Hi") coText
    let choice = convertText(text :: T.Text) :: String

    case choice of 
        "Twelve Bar Blues" -> createSheet twelveBarBlues A octave 4 4
        "Axis of Awesome"-> createSheet axisOfAwesome A octave 4 4
        "Pessimistic" -> createSheet pessimistic A octave 4 4
        "Pop"-> createSheet pop1 A octave 4 4
        "JazzCat" -> createSheet jazzCat A octave 4 4
        "Pachelbel" -> createSheet pachelbel A octave 4 4
    putStrLn (show coText)

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` "\"\'") ]

fixFilePath :: String -> IO(String)
fixFilePath s = do
    putStrLn s
    let a = T.pack ".wav"
    let b = T.pack $ removePunc s
    let hasSuffix = T.isInfixOf a b
    if hasSuffix
        then return (s)
        else return (T.unpack $ T.concat [b, a])

openSelectFolderDialog :: Gtk3.Window -> Gtk3.Label -> IO()
openSelectFolderDialog window fileLabel = do
    defaultPath <- (Gtk3.labelGetText fileLabel) :: IO([Char])
    
    dialog <- Gtk3.fileChooserDialogNew
        (Just $ "Demo of the standart dialog "
            ++ "to select a new file")
        (Just window)
        Gtk3.FileChooserActionSave
        [   ("Cancel", Gtk3.ResponseCancel),
            ("Save", Gtk3.ResponseAccept)
        ]
    fileFilter <- Gtk3.fileFilterNew
    Gtk3.fileFilterSetName fileFilter ".wav"
    Gtk3.fileFilterAddMimeType fileFilter "audio/wav"
    Gtk3.fileChooserAddFilter dialog fileFilter        
    Gtk3.widgetShow dialog
    response <- Gtk3.dialogRun dialog
    case response of
        Gtk3.ResponseAccept -> do
            a <- Gtk3.fileChooserGetFilename dialog
            let fileName = fromMaybe (toFilePath defaultPath) a
            c <- fixFilePath $ show fileName
            Gtk3.labelSetText fileLabel c
        Gtk3.ResponseCancel -> putStrLn "dialog canceled"
        Gtk3.ResponseDeleteEvent -> putStrLn "dialog closed"
    Gtk3.widgetHide dialog

onCustomChecked :: Gtk3.HBox -> Gtk3.Entry -> Gtk3.ComboBox -> Gtk3.CheckButton -> IO ()
onCustomChecked hbox entry comboBox this = do
    status <- Gtk3.toggleButtonGetActive this
    case status of
        True  -> do {
            Gtk3.containerRemove hbox comboBox;
            Gtk3.containerAdd hbox entry
        } 
        False -> do {
            Gtk3.containerRemove hbox entry;
            Gtk3.containerAdd hbox comboBox
        } 
    putStrLn (show status)

