module Main where
import Lib
import Control.Concurrent ( threadDelay )
import Control.Monad ( unless )
import Data.Maybe ( listToMaybe )
import System.Environment ( getArgs )
import Sound.OpenAL
import qualified Graphics.UI.Gtk as Gtk3

type DeviceSpecifier = Maybe String

showDevice :: DeviceSpecifier -> String
showDevice Nothing = "default"
showDevice (Just d) = "'" ++ d ++ "'"

orElse :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElse f g = f >>= maybe g (return . Just)

check :: String -> IO (Maybe a) -> IO a
check what f = f >>= maybe (error $ what ++ " failed") return

boolToMaybe :: Bool -> Maybe ()
boolToMaybe x = if x then Just () else Nothing

getDeviceSpec :: String -> IO [String] -> IO DeviceSpecifier
getDeviceSpec what getter = do
   deviceSpecs <- getter
   unless (null deviceSpecs) $ do
      putStrLn $ "Found " ++ show (length deviceSpecs) ++ " " ++ what ++ ":"
      mapM_ (putStrLn . ("   " ++)) deviceSpecs
   return $ listToMaybe deviceSpecs

main :: IO ()
main = do
   d <- getDeviceSpec "commandline arguments" getArgs `orElse`
        getDeviceSpec "enumerated devices" (get allDeviceSpecifiers)
   putStrLn $ "Using " ++ showDevice d ++ " device"
   device <- check "openDevice" $ openDevice d
   threadDelay 1000000
   check "closeDevice" $ fmap boolToMaybe $ closeDevice device

   -- Gtk3 Gui
   Gtk3.initGUI
   -- Create Window
   window <- Gtk3.windowNew
   Gtk3.on window Gtk3.objectDestroy Gtk3.mainQuit
   Gtk3.set window [ Gtk3.containerBorderWidth Gtk3.:= 10 ]
   hbuttonbox <- Gtk3.hButtonBoxNew
   Gtk3.set window [ Gtk3.containerChild Gtk3.:= hbuttonbox ]

   button1 <- Gtk3.buttonNewWithLabel "Save"
   button2 <- Gtk3.buttonNewWithLabel "Penis"

   Gtk3.set hbuttonbox [ Gtk3.containerChild Gtk3.:= button
                  | button <- [button1, button2] ]

   Gtk3.set hbuttonbox [ Gtk3.buttonBoxLayoutStyle Gtk3.:= Gtk3.ButtonboxStart
                       , Gtk3.buttonBoxChildSecondary button2 Gtk3.:= True  ]

   Gtk3.widgetShowAll window

   Gtk3.mainGUI 
