module Main where
import qualified Graphics.UI.Gtk as G
import Control.Lens
import Gui

main :: IO ()
main = do

    elements <- initialiseGui "VSynth"

    -- Testing some lense functionality
    let win = view window elements
    size <- G.windowGetSize win
    putStrLn "Size of the window was:"
    putStrLn (show size) 