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
import Control.Concurrent.Async
import Control.Concurrent

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

-- Defining some lenses in case we need them later
--window :: Lens' GuiElements Gtk3.Window
--window = lens _window (\guiElements g -> guiElements { _window = g })

window :: Lens' GuiElements Gtk3.Window
window k (GuiElements win hbox che com ent butSave butStart) = (\win' -> GuiElements win' hbox che com ent butSave butStart) <$> k win

hBox :: Lens' GuiElements Gtk3.HBox
hBox k (GuiElements win hbox che com ent butSave butStart) = (\hbox' -> GuiElements win hbox' che com ent butSave butStart) <$> k hbox

checkCustom :: Lens' GuiElements Gtk3.CheckButton
checkCustom k (GuiElements win hbox che com ent butSave butStart) = (\che' -> GuiElements win hbox che' com ent butSave butStart) <$> k che

comboBox :: Lens' GuiElements Gtk3.ComboBox
comboBox k (GuiElements win hbox che com ent butSave butStart) = (\com' -> GuiElements win hbox che com' ent butSave butStart) <$> k com

entChord :: Lens' GuiElements Gtk3.Entry
entChord k (GuiElements win hbox che com ent butSave butStart) = (\ent' -> GuiElements win hbox che com ent' butSave butStart) <$> k ent

butSave :: Lens' GuiElements Gtk3.Button
butSave k (GuiElements win hbox che com ent butSave butStart) = (\butSave' -> GuiElements win hbox che com ent butSave' butStart) <$> k butSave

butStart :: Lens' GuiElements Gtk3.Button
butStart k (GuiElements win hbox che com ent butSave butStart) = (\butStart' -> GuiElements win hbox che com ent butSave butStart') <$> k butStart

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

initialiseGui :: String -> IO (GuiElements)
initialiseGui title = do
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
    hBox4 <- Gtk3.hBoxNew True 5
    hBox5 <- Gtk3.hBoxNew True 5
    button1 <- Gtk3.buttonNewWithLabel "Select output directory"
    button2 <- Gtk3.buttonNewWithLabel "Create File"
    comboBox <- Gtk3.comboBoxNewText
    comboBox2 <- Gtk3.comboBoxNewText
    comboKey <- Gtk3.comboBoxNewText
    entChord <- Gtk3.entryNew
    labChord <- Gtk3.labelNew (Just "Chord progression:")
    labKey <- Gtk3.labelNew (Just "Key:")
    labOctave <- Gtk3.labelNew (Just "Octave:")
    labFolderHead <- Gtk3.labelNew (Just "Filename:")
    labBars <- Gtk3.labelNew (Just "Bars:")
    labNotes <- Gtk3.labelNew (Just "Notes:")
    labFolder <- Gtk3.labelNew (Just filepath)
    checkCustom <- Gtk3.checkButtonNewWithLabel "Custom Mode"
    scaleOct <- Gtk3.hScaleNewWithRange (-5.0) (5.0) (0.1)
    spinBars <- Gtk3.spinButtonNewWithRange 1 20 1
    spinNotes <- Gtk3.spinButtonNewWithRange 1 20 1

    -- Setup GUI elements
    Gtk3.on window Gtk3.objectDestroy Gtk3.mainQuit
    Gtk3.set window [ Gtk3.containerBorderWidth Gtk3.:= 10, Gtk3.windowTitle Gtk3.:= title ]
    Gtk3.entrySetText entChord "1 5 6 4"
    Gtk3.rangeSetValue scaleOct (0.0)
    Gtk3.spinButtonSetValue spinBars 4
    Gtk3.spinButtonSetValue spinNotes 4

    Gtk3.containerAdd vBox hBox2
    Gtk3.containerAdd vBox hBox
    Gtk3.containerAdd vBox hBox4
    Gtk3.containerAdd vBox hBox5
    Gtk3.containerAdd vBox hbuttonbox
    Gtk3.containerAdd vBox hBox3
    Gtk3.containerAdd window vBox
    Gtk3.containerAdd hBox2 checkCustom
    Gtk3.containerAdd hBox labChord
    Gtk3.containerAdd hBox entChord
    Gtk3.containerAdd hBox comboBox2
    Gtk3.containerAdd hBox comboBox
    Gtk3.containerAdd hBox4 labKey
    Gtk3.containerAdd hBox4 comboKey
    Gtk3.containerAdd hBox4 labOctave
    Gtk3.containerAdd hBox4 scaleOct
    Gtk3.containerAdd hBox5 labBars
    Gtk3.containerAdd hBox5 spinBars
    Gtk3.containerAdd hBox5 labNotes
    Gtk3.containerAdd hBox5 spinNotes
    Gtk3.containerAdd hBox3 labFolderHead
    Gtk3.containerAdd hBox3 labFolder
    
    Gtk3.set hbuttonbox [ Gtk3.containerChild Gtk3.:= button
                          | button <- [button1, button2] ]
    Gtk3.set hbuttonbox [ Gtk3.buttonBoxLayoutStyle Gtk3.:= Gtk3.ButtonboxStart
                           , Gtk3.buttonBoxChildSecondary button2 Gtk3.:= True  ]

    sequence $ setupComboBox comboBox ["Twelve Bar Blues", "Axis of Awesome", "Pessimistic", "Pop", "JazzCat", "Pachelbel"]
    sequence $ setupComboBox comboBox2 ["Major", "Minor"]
    sequence $ setupComboBox comboKey ["A", "As", "B", "C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs"]

    Gtk3.comboBoxSetActive comboBox 0
    Gtk3.comboBoxSetActive comboBox2 0
    Gtk3.comboBoxSetActive comboKey 0

    -- Setup event handlers
    Gtk3.on button1 Gtk3.buttonActivated $ openSelectFolderDialog window labFolder
    Gtk3.on button2 Gtk3.buttonActivated $ onStartButtonClicked comboBox comboBox2 labFolder createSheet checkCustom entChord comboKey scaleOct spinBars spinNotes
    Gtk3.on checkCustom Gtk3.toggled (onCustomChecked hBox entChord comboBox checkCustom)

    -- Finish GUI setup
    Gtk3.widgetShowAll window
    Gtk3.containerRemove hBox entChord
    Gtk3.mainGUI

    let elements = GuiElements {_window = window,_hBox = hBox,_checkCustom = checkCustom, _comboBox = comboBox,_entChord = entChord, _butSave = button1, _butStart = button2}
    return elements

chooseChord :: String -> MajMin
chooseChord chordType
    | chordType == "Minor" = Minor
    | otherwise = Major

createChordProgression :: String -> [Int]
createChordProgression input = map (read::String->Int) list
    where list = words input

onStartButtonClicked :: Gtk3.ComboBox -> Gtk3.ComboBox -> Gtk3.Label -> (FilePath -> [Progression] -> Key -> Octave -> MajMin-> Int -> NumNotes -> IO ()) -> Gtk3.CheckButton -> Gtk3.Entry -> Gtk3.ComboBox -> Gtk3.HScale -> Gtk3.SpinButton -> Gtk3.SpinButton -> IO()
onStartButtonClicked comboBox comboBoxMiMa fileLabel createSheet customTB customEnt comboKeys scaleOctave spinBars spinNotes = do
    --spinBars spinNotes
    keysText <- Gtk3.comboBoxGetActiveText comboKeys
    let selKey = convertText((fromMaybe (toText "A") keysText) :: T.Text) :: String
    let key = selectKey selKey

    octaveRangeValue <- Gtk3.rangeGetValue scaleOctave
    let octaveValue = (realToFrac octaveRangeValue) :: Float

    barsValue <- Gtk3.spinButtonGetValueAsInt spinBars
    notesValue <- Gtk3.spinButtonGetValueAsInt spinNotes
    
    status <- Gtk3.toggleButtonGetActive customTB
    entText <- (Gtk3.entryGetText customEnt) :: IO([Char])

    filepath <- (Gtk3.labelGetText fileLabel) :: IO([Char])

    chordTypeText <- Gtk3.comboBoxGetActiveText comboBoxMiMa
    let chordType = convertText((fromMaybe (toText "Major") chordTypeText) :: T.Text) :: String
    let miMa = chooseChord chordType

    coText <- Gtk3.comboBoxGetActiveText comboBox
    let text = fromMaybe (toText "Hi") coText
    let choice = convertText(text :: T.Text) :: String

    if status
        then do
            let chordProg = createChordProgression entText
            async $ createSheet filepath chordProg key octaveValue miMa barsValue notesValue
            return ()
        else do
            case choice of 
                "Twelve Bar Blues" -> do
                    async $ createSheet filepath twelveBarBlues key octaveValue miMa barsValue notesValue
                "Axis of Awesome"-> do
                    async $ createSheet filepath axisOfAwesome key octaveValue miMa barsValue notesValue
                "Pessimistic" -> do 
                    async $ createSheet filepath pessimistic key octaveValue miMa barsValue notesValue
                "Pop"-> do 
                    async $ createSheet filepath pop1 key octaveValue miMa barsValue notesValue
                "JazzCat" -> do 
                    async $ createSheet filepath jazzCat key octaveValue miMa barsValue notesValue
                "Pachelbel" -> do 
                    async $ createSheet filepath pachelbel key octaveValue miMa barsValue notesValue
            return ()

-- data Key = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs deriving (Enum, Show, Eq, Bounded)
selectKey :: String -> Key
selectKey text 
    | text == "A" = A
    | text == "As" = As
    | text == "B" = B
    | text == "C" = C
    | text == "Cs" = Cs
    | text == "D" = D
    | text == "Ds" = Ds
    | text == "E" = E
    | text == "F" = F
    | text == "Fs" = Fs
    | text == "G" = G
    | otherwise = Gs

-- Remove " and ' from text String.
removeParantheses :: String -> String
removeParantheses xs = [ x | x <- xs, not (x `elem` "\"\'") ]

fixFilePath :: String -> String
fixFilePath s 
    | hasSuffix = s
    | otherwise = T.unpack $ T.concat [b,a]
    where   hasSuffix = T.isInfixOf a b
            a = T.pack ".wav"
            b = T.pack $ removeParantheses s

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
            let c = fixFilePath $ show fileName
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

