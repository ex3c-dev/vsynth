module Gui where
import qualified Graphics.UI.Gtk as Gtk3
import qualified Data.Text as T
import Control.Lens

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

--initialiseGui :: ( String -> IO ) -> String -> IO (GuiElements)
initialiseGui :: (String -> IO()) -> String -> IO (GuiElements)
initialiseGui function title = do
    Gtk3.initGUI

    -- Create GUI elements
    window <- Gtk3.windowNew
    hBox <- Gtk3.hBoxNew True 5
    hbuttonbox <- Gtk3.hButtonBoxNew
    vBox <- Gtk3.vBoxNew True 5
    hBox2 <- Gtk3.hBoxNew True 5
    button1 <- Gtk3.buttonNewWithLabel "Select output directory"
    button2 <- Gtk3.buttonNewWithLabel "Create File"
    comboBox <- Gtk3.comboBoxNewText
    entChord <- Gtk3.entryNew
    labChord <- Gtk3.labelNew (Just "Chord progression:")
    checkCustom <- Gtk3.checkButtonNewWithLabel "Custom Mode"

    -- Setup GUI elements
    Gtk3.on window Gtk3.objectDestroy Gtk3.mainQuit
    Gtk3.set window [ Gtk3.containerBorderWidth Gtk3.:= 10, Gtk3.windowTitle Gtk3.:= title ]

    Gtk3.containerAdd vBox hBox2
    Gtk3.containerAdd vBox hBox
    Gtk3.containerAdd vBox hbuttonbox
    Gtk3.containerAdd window vBox

    Gtk3.entrySetText entChord "Enter chord array"

    --Gtk3.comboBoxAppendText comboBox (T.pack "Twelve Bar Blues")
    --Gtk3.comboBoxAppendText comboBox (T.pack "Axis of Awesome")
    --Gtk3.comboBoxAppendText comboBox (T.pack "Pessimistic")
    --Gtk3.comboBoxAppendText comboBox (T.pack "Pop")
    --Gtk3.comboBoxAppendText comboBox (T.pack "JazzCat")
    --Gtk3.comboBoxAppendText comboBox (T.pack "Pachelbel")
    sequence $ setupComboBox comboBox ["Twelve Bar Blues", "Axis of Awesome", "Pessimistic", "Pop", "JazzCat", "Pachelbel"]
    Gtk3.comboBoxSetActive comboBox 0

    Gtk3.containerAdd hBox2 checkCustom
    Gtk3.containerAdd hBox labChord
    Gtk3.containerAdd hBox entChord
    Gtk3.containerAdd hBox comboBox

    Gtk3.set hbuttonbox [ Gtk3.containerChild Gtk3.:= button
                          | button <- [button1, button2] ]

    Gtk3.set hbuttonbox [ Gtk3.buttonBoxLayoutStyle Gtk3.:= Gtk3.ButtonboxStart
                           , Gtk3.buttonBoxChildSecondary button2 Gtk3.:= True  ]

    -- Setup event handlers
    Gtk3.on button1 Gtk3.buttonActivated $ openSelectFolderDialog window
    Gtk3.on button2 Gtk3.buttonActivated $ onStartButtonClicked comboBox
    Gtk3.on checkCustom Gtk3.toggled (onCustomChecked hBox entChord comboBox checkCustom)

    -- Finish GUI setup
    Gtk3.widgetShowAll window
    Gtk3.containerRemove hBox entChord
    Gtk3.mainGUI

    

    let elements = GuiElements {_window = window,_hBox = hBox,_checkCustom = checkCustom, _comboBox = comboBox,_entChord = entChord, _butSave = button1, _butStart = button2}
    return elements

addWidgets ::(Gtk3.WidgetClass widgets, Gtk3.ContainerClass container) => [widgets] -> container -> [IO ()]
addWidgets widgets container = map (\x -> Gtk3.containerAdd (Gtk3.toContainer container) (Gtk3.toWidget x)) widgets

test :: String -> IO ()
test message = do
    putStrLn message

createWindow :: String -> IO ()
createWindow s = do
    windowElements <- initialiseGui (test) s
    
    size <- Gtk3.windowGetSize (_window windowElements)

    -- Adding listeners after gui creation does not work
    --Gtk3.on (_checkCustom windowElements) Gtk3.toggled (onCustomChecked (_hBox windowElements) (_entChord windowElements) (_comboBox windowElements) (_checkCustom windowElements))

    --Gtk3.on _checkCustom windowElements Gtk3.toggled onCustomChecked
    return ()

onStartButtonClicked :: Gtk3.ComboBox -> IO()
onStartButtonClicked comboBox = do
    text <- Gtk3.comboBoxGetActiveText comboBox
    putStrLn (show text)


openSelectFolderDialog :: Gtk3.Window -> IO()
openSelectFolderDialog window = do
    dialog <- Gtk3.fileChooserDialogNew
        (Just $ "Demo of the standart dialog "
            ++ "to select an existing folder")
        (Just window)
        Gtk3.FileChooserActionSelectFolder
        [   ("Yes, this dialog looks 1337", Gtk3.ResponseAccept)
            , ("Raus mit de Viechers!", Gtk3.ResponseCancel)
        ]
    Gtk3.widgetShow dialog
    response <- Gtk3.dialogRun dialog
    case response of
        Gtk3.ResponseAccept -> do {
            Just fileName <- Gtk3.fileChooserGetFilename dialog;
            putStrLn $ "you selected the folder " ++ show fileName;}
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

